; Configuration Variables
(def software-adc 1)
(def min-adc-throttle 0.1)
(def min-adc-brake 0.1)
(def show-batt-in-idle 1)
(def min-speed 1)
(def button-safety-speed (/ 0.1 3.6))

; Speed Mode Configurations
(def eco-speed (/ 7 3.6))
(def eco-current 0.6)
(def eco-watts 400)
(def eco-fw 0)
(def drive-speed (/ 17 3.6))
(def drive-current 0.7)
(def drive-watts 500)
(def drive-fw 0)
(def sport-speed (/ 21 3.6))
(def sport-current 1.0)
(def sport-watts 700)
(def sport-fw 0)

; Secret Mode Configurations
(def secret-enabled 1)
(def secret-eco-speed (/ 27 3.6))
(def secret-eco-current 0.8)
(def secret-eco-watts 1200)
(def secret-eco-fw 0)
(def secret-drive-speed (/ 47 3.6))
(def secret-drive-current 0.9)
(def secret-drive-watts 1500)
(def secret-drive-fw 0)
(def secret-sport-speed (/ 1000 3.6))
(def secret-sport-current 1.0)
(def secret-sport-watts 1500000)
(def secret-sport-fw 10)

; SHU Protocol Constants
(def shu-header 0x55AA)
(def shu-lock-cmd 0x7D)
(def shu-unlock-cmd 0x7E)
(def shu-status-cmd 0x01)

; Import VESC Library
(import "pkg@://vesc_packages/lib_code_server/code_server.vescpkg" 'code-server)
(read-eval-program code-server)

; Xiaomi Protocol Constants
(def xiaomi-header 0x55AA)
(def xiaomi-tail 0x0A)
(def xiaomi-lock-cmd 0x7D)
(def xiaomi-unlock-cmd 0x7E)

; Update UART setup for Xiaomi protocol
(uart-start 115200 'half-duplex)
(gpio-configure 'pin-rx 'pin-mode-in-pu)
(def xiaomi-tx-buf (array-create 64))
(def xiaomi-rx-buf (array-create 64))

; State Variables
(def presstime (systime))
(def presses 0)
(def off 0)
(def lock 0)
(def speedmode 4)
(def light 0)
(def unlock 0)
(def feedback 0)

; ADC Configuration
(if (= software-adc 1)
    (app-adc-detach 3 1)
    (app-adc-detach 3 0)
)

; SHU Protocol Functions
(defun handle-shu-message(code data-len)
    {
        (cond 
            ((= code shu-lock-cmd) 
                {
                    (set 'lock 1)
                    (set 'feedback 1)
                    ; Store lock code if needed
                    (var lock-code (bufget-u16 uart-buf 3))
                    (send-shu-response 0x01)
                })
            ((= code shu-unlock-cmd)
                {
                    ; Verify unlock code if needed
                    (var unlock-code (bufget-u16 uart-buf 3))
                    (set 'lock 0)
                    (set 'feedback 1)
                    (send-shu-response 0x01)
                })
            ((= code shu-status-cmd)
                (send-shu-status))
        )
    }
)

(defun send-shu-response(status)
    {
        (bufset-u16 shu-response 0 shu-header)
        (bufset-u8 shu-response 2 0x02)
        (bufset-u8 shu-response 3 status)
        (var crc (calculate-shu-crc shu-response 4))
        (bufset-u16 shu-response 4 crc)
        (uart-write shu-response)
    }
)

(defun send-shu-status()
    {
        (bufset-u16 shu-response 0 shu-header)
        (bufset-u8 shu-response 2 0x04)
        (bufset-u8 shu-response 3 (if (= lock 1) 0x01 0x00))
        (bufset-u8 shu-response 4 speedmode)
        (bufset-u8 shu-response 5 (if (= light 1) 0x01 0x00))
        (var crc (calculate-shu-crc shu-response 6))
        (bufset-u16 shu-response 6 crc)
        (uart-write shu-response)
    }
)

(defun calculate-shu-crc(buffer length)
    {
        (var crc 0)
        (looprange i 2 length
            (set 'crc (+ crc (bufget-u8 buffer i))))
        (bitwise-xor crc 0xFFFF)
    }
)

; Core Functions
(defun adc-input(buffer)
    {
        (let ((current-speed (* (get-speed) 3.6))
            (throttle (/(bufget-u8 uart-buf 4) 77.2)) 
            (brake (/(bufget-u8 uart-buf 5) 77.2)))
            {
                (if (< throttle 0)
                    (setf throttle 0))
                (if (> throttle 3.3)
                    (setf throttle 3.3))
                (if (< brake 0)
                    (setf brake 0))
                (if (> brake 3.3)
                    (setf brake 3.3))
                
                (app-adc-override 0 throttle)
                (app-adc-override 1 brake)
            }
        )
    }
)

(defun handle-features()
    {
        (if (or (or (= off 1) (= lock 1) (< (* (get-speed) 3.6) min-speed)))
            (if (not (app-is-output-disabled))
                {
                    (app-adc-override 0 0)
                    (app-adc-override 1 0)
                    (app-disable-output -1)
                    (set-current 0)
                }
            )
            (if (app-is-output-disabled)
                (app-disable-output 0)
            )
        )
        
        (if (= lock 1)
            {
                (set-current-rel 0)
                (if (> (* (get-speed) 3.6) min-speed)
                    (set-brake-rel 1)
                    (set-brake-rel 0)
                )
            }
        )
    }
)

(defun update-dash(buffer) ; Frame 0x64
    {
        (var current-speed (* (l-speed) 3.6))
        (var battery (*(get-batt) 100))

        ; mode field (1=drive, 2=eco, 4=sport, 8=charge, 16=off, 32=lock)
        (if (= off 1)
            (bufset-u8 tx-frame 6 16)
            (if (= lock 1)
                (bufset-u8 tx-frame 6 32) ; lock display
                (if (or (> (get-temp-fet) 60) (> (get-temp-mot) 60)) ; temp icon will show up above 60 degree
                    (bufset-u8 tx-frame 6 (+ 128 speedmode))
                    (bufset-u8 tx-frame 6 speedmode)
                )
            )
        )
        
        ; batt field
        (bufset-u8 tx-frame 7 battery)

        ; light field
        (if (= off 0)
            (bufset-u8 tx-frame 8 light)
            (bufset-u8 tx-frame 8 0)
        )
        
        ; beep field
        (if (= lock 1)
            (if (> current-speed min-speed)
                (bufset-u8 tx-frame 9 1) ; beep lock
                (bufset-u8 tx-frame 9 0))
            (if (> feedback 0)
                {
                    (bufset-u8 tx-frame 9 1)
                    (set 'feedback (- feedback 1))
                }
                (bufset-u8 tx-frame 9 0)
            )
        )

        ; speed field
        (if (= (+ show-batt-in-idle unlock) 2)
            (if (> current-speed 1)
                (bufset-u8 tx-frame 10 current-speed)
                (bufset-u8 tx-frame 10 battery))
            (bufset-u8 tx-frame 10 current-speed)
        )
        
        ; error field
        (bufset-u8 tx-frame 11 (get-fault))

        ; calc crc
        (var crc 0)
        (looprange i 2 12
            (set 'crc (+ crc (bufget-u8 tx-frame i))))
        (var c-out (bitwise-xor crc 0xFFFF)) 
        (bufset-u8 tx-frame 12 c-out)
        (bufset-u8 tx-frame 13 (shr c-out 8))
        
        ; write
        (uart-write tx-frame)
    }
)

; Xiaomi Protocol Functions
(defun xiaomi-calculate-checksum (buffer len)
    {
        (var sum 0)
        (looprange i 2 len
            (set 'sum (+ sum (bufget-u8 buffer i))))
        (bitwise-and sum 0xFF)
    }
)

(defun xiaomi-send-packet (cmd data len)
    {
        (bufset-u16 xiaomi-tx-buf 0 xiaomi-header)
        (bufset-u8 xiaomi-tx-buf 2 len)
        (bufset-u8 xiaomi-tx-buf 3 cmd)
        (if (> len 0)
            (looprange i 0 len
                (bufset-u8 xiaomi-tx-buf (+ i 4) (bufget-u8 data i))))
        (bufset-u8 xiaomi-tx-buf (+ len 4) (xiaomi-calculate-checksum xiaomi-tx-buf (+ len 4)))
        (bufset-u8 xiaomi-tx-buf (+ len 5) xiaomi-tail)
        (uart-write xiaomi-tx-buf (+ len 6))
    }
)

(defun xiaomi-handle-packet ()
    {
        (var cmd (bufget-u8 xiaomi-rx-buf 3))
        (var len (bufget-u8 xiaomi-rx-buf 2))
        
        (cond 
            ((= cmd xiaomi-lock-cmd)
                {
                    (set 'lock 1)
                    (set 'feedback 1)
                    (app-disable-output -1)
                    (xiaomi-send-packet 0x01 0 0) ; ACK
                })
            ((= cmd xiaomi-unlock-cmd)
                {
                    (set 'lock 0)
                    (set 'feedback 1)
                    (app-disable-output 0)
                    (xiaomi-send-packet 0x01 0 0) ; ACK
                })
        )
    }
)


(defun read-frames()
    (loopwhile t
        {
            (uart-read-bytes xiaomi-rx-buf 3 0)
            (if (= (bufget-u16 xiaomi-rx-buf 0) xiaomi-header)
                {
                    (var len (bufget-u8 xiaomi-rx-buf 2))
                    (if (and (> len 0) (< len 60))
                        {
                            (uart-read-bytes xiaomi-rx-buf (+ len 3) 0)
                            (var checksum (xiaomi-calculate-checksum xiaomi-rx-buf (+ len 4)))
                            (if (and 
                                (= (bufget-u8 xiaomi-rx-buf (+ len 4)) checksum)
                                (= (bufget-u8 xiaomi-rx-buf (+ len 5)) xiaomi-tail))
                                {
                                    (xiaomi-handle-packet)
                                    (handle-frame (bufget-u8 xiaomi-rx-buf 1))
                                }
                            )
                        }
                    )
                }
            )
        }
    )
)


(defun handle-frame(code)
    {
        (if (and (= code 0x65) (= software-adc 1))
            (adc-input uart-buf)
        )
        
        (update-dash uart-buf)
    }
)

(defun handle-button()
    (if (= presses 1) ; single press
        (if (= off 1) ; is it off? turn on scooter again
            {
                (set 'off 0) ; turn on
                (set 'feedback 1) ; beep feedback
                (set 'unlock 0) ; Disable unlock on turn off
                (apply-mode) ; Apply mode on start-up
                (stats-reset) ; reset stats when turning on
            }
            (set 'light (bitwise-xor light 1)) ; toggle light
        )
        (if (>= presses 2) ; double press
            {
                (if (> (get-adc-decoded 1) min-adc-brake) ; if brake is pressed
                    (if (and (= secret-enabled 1) (> (get-adc-decoded 0) min-adc-throttle))
                        {
                            (set 'unlock (bitwise-xor unlock 1))
                            (set 'feedback 2) ; beep 2x
                            (apply-mode)
                        }
                        {
                            (set 'unlock 0)
                            (apply-mode)
                            (set 'lock (bitwise-xor lock 1)) ; lock on or off
                            (set 'feedback 1) ; beep feedback
                        }
                    )
                    {
                        (if (= lock 0)
                            {
                                (cond
                                    ((= speedmode 1) (set 'speedmode 4))
                                    ((= speedmode 2) (set 'speedmode 1))
                                    ((= speedmode 4) (set 'speedmode 2))
                                )
                                (apply-mode)
                            }
                        )
                    }
                )
            }
        )
    )
)

(defun handle-holding-button()
    {
        (if (= (+ lock off) 0) ; it is locked and off?
            {
                (set 'unlock 0) ; Disable unlock on turn off
                (apply-mode)
                (set 'off 1) ; turn off
                (set 'light 0) ; turn off light
                (set 'feedback 1) ; beep feedback
            }
        )
    }
)

(defun reset-button()
    {
        (set 'presstime (systime)) ; reset press time again
        (set 'presses 0)
    }
)

; Speed mode implementation

(defun apply-mode()
    (if (= unlock 0)
        (if (= speedmode 1)
            (configure-speed drive-speed drive-watts drive-current drive-fw)
            (if (= speedmode 2)
                (configure-speed eco-speed eco-watts eco-current eco-fw)
                (if (= speedmode 4)
                    (configure-speed sport-speed sport-watts sport-current sport-fw)
                )
            )
        )
        (if (= speedmode 1)
            (configure-speed secret-drive-speed secret-drive-watts secret-drive-current secret-drive-fw)
            (if (= speedmode 2)
                (configure-speed secret-eco-speed secret-eco-watts secret-eco-current secret-eco-fw)
                (if (= speedmode 4)
                    (configure-speed secret-sport-speed secret-sport-watts secret-sport-current secret-sport-fw)
                )
            )
        )
    )
)

(defun configure-speed(speed watts current fw)
    {
        (set-param 'max-speed speed)
        (set-param 'l-watt-max watts)
        (set-param 'l-current-max-scale current)
        (set-param 'foc-fw-current-max fw)
    }
)

(defun set-param (param value)
    {
        (conf-set param value)
        (loopforeach id (can-list-devs)
            (looprange i 0 5 {
                (if (eq (rcode-run id 0.1 `(conf-set (quote ,param) ,value)) t) (break t))
                false
            })
        )
    }
)

(defun l-speed()
    {
        (var l-speed (get-speed))
        (loopforeach i (can-list-devs)
            {
                (var l-can-speed (canget-speed i))
                (if (< l-can-speed l-speed)
                    (set 'l-speed l-can-speed)
                )
            }
        )

        l-speed
    }
)

(defun button-logic()
    {
        ; Assume button is not pressed by default
        (var buttonold 0)
        (loopwhile t
            {
                (var button (gpio-read 'pin-rx))
                (sleep 0.05) ; wait 50 ms to debounce
                (var buttonconfirm (gpio-read 'pin-rx))
                (if (not (= button buttonconfirm))
                    (set 'button 0)
                )
                
                (if (> buttonold button)
                    {
                        (set 'presses (+ presses 1))
                        (set 'presstime (systime))
                    }
                    (button-apply button)
                )
                
                (set 'buttonold button)
                (handle-features)
            }
        )
    }
)

(defun button-apply(button)
    {
        (var time-passed (- (systime) presstime))
        (var is-active (or (= off 1) (<= (get-speed) button-safety-speed)))

        (if (> time-passed 2500) ; after 2500 ms
            (if (= button 0) ; check button is still pressed
                (if (> time-passed 6000) ; long press after 6000 ms
                    {
                        (if is-active
                            (handle-holding-button)
                        )
                        (reset-button) ; reset button
                    }
                )
                (if (> presses 0) ; if presses > 0
                    {
                        (if is-active
                            (handle-button) ; handle button presses
                        )
                        (reset-button) ; reset button
                    }
                )
            )
        )
    }
)

; Initialize and Start
(apply-mode)
(spawn 150 read-frames)
(button-logic)
