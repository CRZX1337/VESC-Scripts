; Xiaomi Scooter VESC Integration Script
; Mit Lock-/Unlock-Unterstützung und Debugging-Ausgaben
; Importiere die VESC-Bibliothek
(import "pkg@://vesc_packages/lib_code_server/code_server.vescpkg" 'code-server)
(read-eval-program code-server)

; Konfigurationsvariablen
(def software-adc 1)
(def min-adc-throttle 0.1)
(def min-adc-brake 0.1)
(def show-batt-in-idle 1)
(def min-speed 1)
(def button-safety-speed (/ 0.1 3.6))

; Geschwindigkeitsmodi
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

; Secret Mode Konfigurationen
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

; SHU Protokoll Konstanten
(def shu-header 0x55AA)
(def shu-lock-cmd 0x7D)
(def shu-unlock-cmd 0x7E)
(def shu-status-cmd 0x01)

; Debugging-Funktion
(defun log (message)
  (format t "~A~%" message))

; UART-Schreibfunktion
(defun write-to-uart (command)
  (log (format nil "UART gesendet: ~A" command)))

; Funktion zum Senden eines UART-Befehls mit Debugging
(defun send-uart-command (command)
  (write-to-uart command)
  (log (format nil "Befehl gesendet: ~A" command)))

; Funktion zum Verarbeiten von SHU-Nachrichten
(defun handle-shu-message (code data-len)
  (cond
    ((= code shu-lock-cmd)
     (progn
       (set 'lock 1)
       (set 'feedback 1)
       (log "Lock-Befehl empfangen")
       (send-shu-response 0x01)))
    ((= code shu-unlock-cmd)
     (progn
       (set 'lock 0)
       (set 'feedback 1)
       (log "Unlock-Befehl empfangen")
       (send-shu-response 0x01)))
    ((= code shu-status-cmd)
     (send-shu-status))
    (t
     (log "Unbekannter SHU-Befehl empfangen"))))

(defun send-shu-response (status)
  (bufset-u16 shu-response 0 shu-header)
  (bufset-u8 shu-response 2 0x02)
  (bufset-u8 shu-response 3 status)
  (var crc (calculate-shu-crc shu-response 4))
  (bufset-u16 shu-response 4 crc)
  (uart-write shu-response)
  (log (format nil "SHU-Antwort gesendet mit Status: ~A" status)))

(defun send-shu-status ()
  (bufset-u16 shu-response 0 shu-header)
  (bufset-u8 shu-response 2 0x04)
  (bufset-u8 shu-response 3 (if (= lock 1) 0x01 0x00))
  (bufset-u8 shu-response 4 speedmode)
  (bufset-u8 shu-response 5 (if (= light 1) 0x01 0x00))
  (var crc (calculate-shu-crc shu-response 6))
  (bufset-u16 shu-response 6 crc)
  (uart-write shu-response)
  (log "SHU-Status gesendet"))

(defun calculate-shu-crc (buffer length)
  (var crc 0)
  (looprange i 2 length
    (set 'crc (+ crc (bufget-u8 buffer i))))
  (bitwise-xor crc 0xFFFF))

; Hauptfunktionen
(defun adc-input (buffer)
  (let ((current-speed (* (get-speed) 3.6))
        (throttle (/(bufget-u8 uart-buf 4) 77.2))
        (brake (/(bufget-u8 uart-buf 5) 77.2)))
    (if (< throttle 0) (setf throttle 0))
    (if (> throttle 3.3) (setf throttle 3.3))
    (if (< brake 0) (setf brake 0))
    (if (> brake 3.3) (setf brake 3.3))
    (app-adc-override 0 throttle)
    (app-adc-override 1 brake)))

(defun handle-features ()
  (if (or (= off 1) (= lock 1) (< (* (get-speed) 3.6) min-speed))
      (if (not (app-is-output-disabled))
          (progn
            (app-adc-override 0 0)
            (app-adc-override 1 0)
            (app-disable-output -1)
            (set-current 0))))
  (if (app-is-output-disabled)
      (app-disable-output 0))
  (if (= lock 1)
      (progn
        (set-current-rel 0)
        (if (> (* (get-speed) 3.6) 0)
            (log "Lock aktiviert, Geschwindigkeit > 0")))))
