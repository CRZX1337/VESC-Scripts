# Contributing to VESC SCRIPTS

We welcome contributions to the VESC SCRIPTS project!  Whether you're a seasoned developer, a Lisp enthusiast, or simply a scooter enthusiast with ideas, your help is valuable.

## How to Contribute

There are many ways you can contribute to this project:

*   **Reporting Bugs:** If you encounter any issues or unexpected behavior while using the scripts, please let us know! Clear and detailed bug reports are incredibly helpful.
*   **Suggesting Features:** Have a great idea for a new feature or enhancement? We'd love to hear it!  Share your suggestions and let's discuss them.
*   **Writing Code:**  If you're comfortable with Lisp scripting and VESC, you can contribute by:
    *   Fixing existing bugs.
    *   Implementing new features.
    *   Improving the performance or efficiency of the scripts.
    *   Adding new scripts for different functionalities.
*   **Improving Documentation:**  Help make the project more accessible by improving the documentation. This could include:
    *   Clarifying existing documentation.
    *   Adding examples and usage instructions.
    *   Creating tutorials or guides.
*   **Testing:**  Testing new scripts or features and providing feedback is crucial to ensure stability and quality.

## Getting Started

1.  **Fork the Repository:** Click the "Fork" button at the top right of the repository page on GitHub. This will create a copy of the repository in your own GitHub account.

2.  **Clone Your Fork:** Clone your forked repository to your local machine using Git:

    ```bash
    git clone https://github.com/YOUR_USERNAME/VESC-Scripts.git
    cd VESC-Scripts
    ```
    Replace `YOUR_USERNAME` with your GitHub username.

3.  **Create a Branch:** Before making any changes, create a new branch for your contribution.  Use a descriptive branch name, such as `feature/new-cruise-control` or `bugfix/dashboard-error`:

    ```bash
    git checkout -b feature/your-feature-name
    ```

4.  **Make Your Changes:**  Implement your bug fix, feature, or documentation improvement.

5.  **Test Your Changes:**  Thoroughly test your changes to ensure they work as expected and don't introduce any regressions.  If possible, test on your VESC and scooter setup.

6.  **Commit Your Changes:** Commit your changes with clear and concise commit messages. Follow these guidelines for commit messages:

    *   Use the present tense ("Add feature" not "Added feature").
    *   Use the imperative mood ("Fix bug" not "Fixes bug").
    *   Limit the first line to 72 characters or less.
    *   Reference any relevant issue numbers in the commit message (e.g., "Fixes #123").

    ```bash
    git add .
    git commit -m "Add descriptive commit message here"
    ```

7.  **Push Your Branch:** Push your branch to your forked repository on GitHub:

    ```bash
    git push origin feature/your-feature-name
    ```

8.  **Create a Pull Request:**  Go to the original VESC-Scripts repository on GitHub. You should see a prompt to "Compare & pull request" for your newly pushed branch. Click this button to create a pull request.

9.  **Describe Your Pull Request:** In the pull request description, clearly explain:
    *   What changes you've made.
    *   Why you made these changes.
    *   Any relevant context or background information.
    *   If it addresses a specific issue, link to that issue (e.g., "Closes #123").

10. **Wait for Review:**  Your pull request will be reviewed by project maintainers. They may provide feedback or request changes. Be prepared to discuss your changes and make adjustments as needed.

11. **Merge:** Once your pull request is approved and all checks pass, it will be merged into the main branch. Congratulations, you've contributed to VESC SCRIPTS!

## Coding Style and Conventions

While we don't have strict coding style guidelines for Lisp scripts at this time, please try to follow these general best practices:

*   **Readability:** Write code that is easy to understand. Use meaningful variable and function names.
*   **Comments:** Add comments to explain complex logic or important parts of your code.
*   **Keep it Concise:**  Strive for clear and concise code. Avoid unnecessary complexity.
*   **Testability:**  Write code that is easy to test (even if formal tests are not yet in place).

## Reporting Bugs

When reporting bugs, please provide as much detail as possible to help us understand and reproduce the issue. Include the following information:

*   **Script Version:** Specify which version of the VESC SCRIPTS you are using.
*   **VESC Firmware Version:**  Mention the firmware version running on your VESC.
*   **Scooter Model:**  Indicate whether you are using a Xiaomi or Ninebot scooter (and specific model if possible).
*   **Steps to Reproduce:**  Clearly describe the steps you took to encounter the bug.
*   **Expected Behavior:**  Explain what you expected to happen.
*   **Actual Behavior:** Describe what actually happened (the bug).
*   **Error Messages (if any):** Include any error messages you encountered.
*   **Relevant Screenshots or Logs:**  If possible, provide screenshots or logs that illustrate the issue.

You can report bugs by opening a new issue on the GitHub repository.

## Suggesting Features

We welcome feature suggestions! When suggesting a new feature, please:

*   **Describe the Feature:** Clearly explain what the feature would do and how it would benefit users.
*   **Provide Use Cases:**  Give examples of scenarios where this feature would be useful.
*   **Consider Alternatives:**  If you've considered alternative approaches, briefly mention them and why you think your suggestion is the best option.

You can suggest features by opening a new issue on the GitHub repository, clearly marking it as a "feature request."

## Code of Conduct

We strive to maintain a welcoming and inclusive community. Please be respectful and considerate of others when contributing to this project.  We expect all contributors to adhere to a basic standard of respectful communication and collaboration.

Thank you for your contributions! We appreciate your help in making VESC SCRIPTS a better project for everyone.
