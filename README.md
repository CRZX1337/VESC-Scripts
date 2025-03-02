<h1 align="center" id="title">VESC SCRIPTS</h1>

<p align="center">
  <strong>Enhanced VESC Functionality for Xiaomi & Ninebot Scooters with Lisp Scripting</strong>
</p>
<p align="center">
  Unlock advanced customization and integration for your electric scooter using these Lisp scripts designed for VESC motor controllers.
</p>

<br/>

<div align="center">

  [![Dashboard Integration Badge](https://img.shields.io/badge/Dashboard-Integrated-brightgreen?style=flat-square)](https://github.com/CRzx1337/VESC-Scripts/#dashboard-integration)
  [![Customizable Features Badge](https://img.shields.io/badge/Customizable-Extensive-blue?style=flat-square)](https://github.com/CRzx1337/VESC-Scripts/#customizable-features)
  [![License Badge](https://img.shields.io/badge/License-MIT-yellow?style=flat-square)](LICENSE) <!-- Replace LICENSE with your actual license file name -->
  [![Contributions Welcome Badge](https://img.shields.io/badge/Contributions-Welcome-orange?style=flat-square)](https://github.com/CRzx1337/VESC-Scripts/#contributing)
  <!-- Add more badges as needed (e.g., last commit, language, etc.) -->

</div>

<br/>

This repository provides Lisp scripts to extend the capabilities of VESC (Vedder Electronic Speed Controller) motor controllers when used with Xiaomi and Ninebot electric scooters.  These scripts empower you to go beyond the limitations of standard VESC firmware, enabling deeper customization and seamless integration.

<br/>

<h2 align="center" id="dashboard-integration">DASHBOARD INTEGRATION</h2>

<p align="center">
  Experience seamless communication between your VESC and the original Xiaomi/Ninebot scooter dashboard.
  Display real-time crucial information directly where you need it:
</p>

<ul align="center">
  <li>Speed</li>
  <li>Battery Level (Percentage)</li>
  <li>Riding Mode</li>
</ul>

<p align="center">
  <strong>Plus, full support with the Scooter Hacking Utility (SHU) App!</strong>  Configure and monitor your VESC scripts directly through SHU for ultimate convenience.
</p>

<br/>

<h2 align="center" id="features">FEATURES</h2>

This project delivers a range of enhancements to your scooter experience:

* **Cruise Control:** Enjoy effortless cruising at your desired speed. (Enable in App settings > ADC > General > Buttons Inputs Enable Cruise Control)
* **Lock/Unlock with SHU (WIP):** Secure your scooter remotely using the Scooter Hacking Utility App. (Work In Progress - Stay tuned!)
* **Always Show Battery Percentage on Idle:**  Keep track of your battery level even when stationary.
* **Brake While Standing Battery Indication:**
    *  Braking while stationary displays a single "cell" battery icon on the dashboard.
    *  This visually represents a voltage threshold (e.g., 3.843v > 38 on dash).
* **Brake + Throttle While Standing Distance Traveled:**  Access your trip distance information even when stationary by combining brake and throttle input.
* **MOSFET Temperature Error Indication:**  Receive critical alerts!  MOSFET temperature is displayed as a distinct ERROR code (RED NUMBERS) on the dashboard, providing early warnings of potential overheating.

<br/>

<h2 align="center" id="customizable-features">CUSTOMIZABLE FEATURES</h2>

Tailor your scooter's performance and behavior to your exact preferences:

* **Eco Modes for Extended Range:** Implement multiple Eco modes with adjustable power limits to maximize battery life for longer rides.
* **Speed Limiters for Safety:** Set custom speed limits for different riding scenarios or user profiles, enhancing safety and control.
* **Custom Riding Modes:** Design unique riding modes with distinct acceleration, braking, and speed characteristics to match your riding style and environment.
* **Advanced Diagnostics and Error Reporting:** Gain deeper insights into your scooter's health with enhanced diagnostic data and more informative error reporting on the dashboard.

<br/>

<h2 align="center" id="getting-started">GETTING STARTED</h2>

Ready to enhance your scooter? Here's a quick guide:

1. **Prerequisites:**
    * VESC motor controller properly installed and configured on your Xiaomi or Ninebot scooter.
    * Scooter Hacking Utility (SHU) App installed on your mobile device.
    * Basic understanding of VESC configuration and Lisp scripting (helpful but not mandatory).

2. **Installation:**
    * **Download the scripts:** Clone or download this repository to your local machine.
    * **Copy to VESC:** Transfer the desired Lisp scripts to your VESC's file system (refer to your VESC documentation for instructions on file access).
    * **Configure VESC:**  Load and configure the scripts within your VESC Tool or SHU App (depending on your VESC firmware and setup).
    * **Customize (Optional):**  Modify the script parameters to adjust features to your liking (e.g., Eco mode power levels, speed limits).

3. **Explore and Enjoy!**  Refer to the individual script documentation (if provided) for specific usage instructions and customization options.

<br/>

<h2 align="center" id="credits">CREDITS</h2>

This project is built upon and greatly inspired by the pioneering work of <strong>1Zuna</strong>, specifically the excellent <a href="https://github.com/m365fw/vesc_m365_dash"><code>vesc_m365_dash</code></a> project.  We extend our sincere gratitude for their foundational contributions to the VESC scooter ecosystem.

<br/>

<h2 align="center" id="disclaimer">DISCLAIMER</h2>

<strong>Proceed with Caution!</strong> Modifying your VESC and scooter firmware involves inherent risks. Incorrect modifications may void warranties and could potentially damage your hardware.

<strong>By using these scripts, you acknowledge and accept full responsibility for any consequences, including but not limited to hardware damage, warranty voidance, or personal injury.</strong>

**Always double-check your configurations, proceed step-by-step, and refer to reliable documentation before making any changes to your VESC or scooter.**

<br/>

<h2 align="center" id="contributing">CONTRIBUTING</h2>

We enthusiastically welcome contributions to this project!  Whether you're a seasoned Lisp programmer or just starting out, your help is valuable.

**How you can contribute:**

* **Bug Reports:**  If you encounter issues, please open a detailed issue report describing the problem and steps to reproduce it.
* **Feature Requests:**  Have a brilliant idea for a new feature?  Share your suggestions and let's discuss them!
* **Code Contributions:**  Fork this repository, implement your improvements or features, and submit a pull request.
* **Documentation:**  Help us improve the documentation, making it clearer and more comprehensive for all users.

**Please review our <a href="CONTRIBUTING.md">Contributing Guidelines</a> for more detailed information on how to contribute.** (Create a `CONTRIBUTING.md` file in your repository root if you haven't already.)

<br/>

<h2 align="center" id="license">LICENSE</h2>

This project is licensed under the **MIT License**. See the <a href="LICENSE"><code>LICENSE</code></a> file for more details. <!-- Replace LICENSE with your actual license file name -->

<br/>

<div align='center'>
  <a href='https://github.com/CRzx1337/VESC-Scripts/'><img src='https://www.websitecounterfree.com/c.php?d=5&id=64815&s=6' border='0' alt='Free Website Counter'></a><br />
  <small>Website Counter</small>
</div>
