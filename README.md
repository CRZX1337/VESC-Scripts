<h1 align="center" id="title">VESC SCRIPTS</h1>

<p align="center">
  <strong>Level Up Your Xiaomi & Ninebot Scooter with VESC Scripts! 🚀</strong>
</p>
<p align="center">
  These scripts unlock some seriously cool extra stuff for your scooter when you're using a VESC motor controller. Think more customization and tighter integration than you get stock!
</p>

<br/>

<div align="center">

  [![Dashboard Integration Badge](https://img.shields.io/badge/Dashboard-Integrated-brightgreen?style=flat-square)](https://github.com/CRzx1337/VESC-Scripts/#dashboard-integration)
  [![Customizable Features Badge](https://img.shields.io/badge/Customizable-Tons-blue?style=flat-square)](https://github.com/CRzx1337/VESC-Scripts/#customizable-features)
  [![License Badge](https://img.shields.io/badge/License-MIT-yellow?style=flat-square)](LICENSE)
  [![Contributions Welcome Badge](https://img.shields.io/badge/Contributions-Welcome-orange?style=flat-square)](https://github.com/CRzx1337/VESC-Scripts/#contributing)

</div>

<br/>

Basically, this repo is for Lisp scripts that make your VESC motor controller way more awesome on Xiaomi and Ninebot scooters.  We're talking about going beyond what the regular VESC firmware can do, for deeper tweaks and better scooter smarts.

<br/>

<h2 align="center" id="dashboard-integration">DASHBOARD LOVE ❤️</h2>

<p align="center">
  Get your VESC talking smoothly with your original Xiaomi/Ninebot dashboard! See all the important stuff right where you're used to:
</p>

<ul align="center">
  <li>Speed</li>
  <li>Battery Level (%)</li>
  <li>Riding Mode</li>
</ul>

<p align="center">
  <strong>Plus, it all works great with the Scooter Hacking Utility (SHU) App!</strong>  Configure and keep an eye on your VESC scripts super easily with SHU.
</p>

<br/>

<h2 align="center" id="features">FEATURES - What You Get! 😎</h2>

Here's the cool stuff these scripts bring to your scooter:

*   **Cruise Control:**  Set it and forget it! Cruise at the speed you want. (Enable in App settings > ADC > General > Buttons Inputs Enable Cruise Control)
*   **Lock/Unlock with SHU (WIP):**  Lock and unlock your scooter remotely using the Scooter Hacking Utility App. (Still working on this one - coming soon!)
*   **Always Show Battery Percentage on Idle:**  Battery percentage is always there on the dash, even when you're stopped.
*   **Brake Battery Meter Magic:**
    *   When you brake while standing still, the dash shows just one "cell" on the battery meter.
    *   This is a visual thing to show a voltage level (like 3.843v > 38 on dash).
*   **Standing Distance Display:** Brake + Throttle while stopped shows how far you've traveled. Handy!
*   **MOSFET Temp ERROR Alerts:**  If your MOSFETs get too hot, you'll see a RED ERROR code on the dash (red numbers).  Early warning system!
*   **⚡ Secret Speed Modes! 🤫**  Unlock hidden performance!
    *   **How to Activate:** Double press the button WHILE holding both the brake and throttle levers.
    *   **What it does:**  Unleashes higher speed and power settings for Eco, Drive, and Sport modes. Think "Ludicrous Speed" for your scooter! 😉
    *   **Use with CAUTION:** These modes are powerful! Make sure your scooter can handle it and be safe!

<br/>

<h2 align="center" id="customizable-features">MAKE IT YOUR OWN 🔧</h2>

You can tweak these scripts to get your scooter just how you like it:

*   **Eco Modes for Max Range:**  Create different Eco modes to go further on a charge.
*   **Speed Limits for Safety:** Set speed limits for different situations or riders.
*   **Custom Ride Modes:**  Design your own riding modes with unique acceleration, braking, and speed feels.
*   **Better Diagnostics:** Get more info and error messages on your dash to keep your scooter healthy.

<br/>

<h2 align="center" id="getting-started">GETTING STARTED - Let's Do This! 🚀</h2>

Ready to upgrade your ride? Quick steps:

1.  **Stuff You Need:**
    *   VESC motor controller installed on your Xiaomi or Ninebot scooter (you probably know this!).
    *   Scooter Hacking Utility (SHU) App on your phone.
    *   A little bit of VESC and Lisp knowledge is helpful, but not a must-have.

2.  **Install Time:**
    *   **Grab the Scripts:** Download them right here:
        *   **Xiaomi Scooters:** [xiaomi.lisp](https://github.com/CRZX1337/VESC-Scripts/blob/main/xiaomi.lisp)
        *   **Ninebot Scooters:** [ninebot.lisp](https://github.com/CRZX1337/VESC-Scripts/blob/main/ninebot.lisp)
        *   Or, grab the whole repo as a ZIP or clone it.
    *   **VESC Transfer:**  Copy the script you want to your VESC (check your VESC docs for how to do files).
    *   **VESC Setup:** Load and set up the script in VESC Tool or SHU App (depends on your VESC setup).
    *   **Customize (Optional):**  Mess with the script settings to change things like Eco mode power, speed limits, etc.

3.  **Ride and Enjoy!** Check out any extra docs for the scripts if there are special instructions.

<br/>

<h2 align="center" id="credits">CREDITS - Big Thanks! 🙏</h2>

Big shoutout to <strong>1Zuna</strong>! This project is built on top of their awesome <a href="https://github.com/m365fw/vesc_m365_dash"><code>vesc_m365_dash</code></a> project.  Massive thanks for the foundation!

<br/>

<h2 align="center" id="disclaimer">DISCLAIMER - Heads Up! ⚠️</h2>

<strong>Listen up!</strong> Messing with your VESC and scooter firmware can be risky. You could void your warranty or even mess up your scooter if you're not careful.

<strong>By using these scripts, you're doing it at your own risk.  If anything breaks, it's on you!</strong>

**Double-check everything, go slowly, and read up on stuff before you change anything on your VESC or scooter.**

<br/>

<h2 align="center" id="contributing">CONTRIBUTING - Join In! 🤝</h2>

Want to help make this even better? Contributions are super welcome!

**Ways to Contribute:**

*   **Bug Reports:** Find a bug? Let us know! Tell us exactly what happened and how to make it happen again.
*   **Feature Ideas:**  Got a cool feature idea? Share it!
*   **Code Help:** If you know Lisp and VESC stuff, jump in and help with code!
*   **Docs Help:** Make the docs clearer and easier to understand.

**Check out the <a href="CONTRIBUTING.md">Contributing Guidelines</a> for more info on how to contribute.**

<br/>

<h2 align="center" id="license">LICENSE - Open Source FTW! 🤘</h2>

This project is under the **MIT License**.  See the <a href="LICENSE"><code>LICENSE</code></a> file for all the legal details.

<br/>

<div align='center'>
  <a href='https://github.com/CRZX1337/VESC-Scripts/'><img src='https://www.websitecounterfree.com/c.php?d=5&id=64815&s=6' border='0' alt='Free Website Counter'></a><br />
  <small>Website Counter</small>
</div>
