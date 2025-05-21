# PSU.Ada.GameEngine

https://github.com/users/apostasy/projects/1/views/1

# Getting Started
## Initial Setup
### Alire
Download from https://alire.ada.dev/ and install

After installation, run `alr version` to ensure alr has been added to system PATH/Environment Variables

### Visual Studio Code
Download from https://code.visualstudio.com/ and install

### Clone the repo

This repository: `git clone mailto:git@github.com:apostasy/PSU.Ada.GameEngine.git`

## Using the library

* You can use the Demos folder as an example
* Create a new binary project within this project with `alr init project_title`
* Add a pin to your project's alire.toml file for this library
    ```
    [[pins]] 
    psu_ada_gameengine = { path='..' } 
    ```
* Reference the library in your project's gpr file
    ```
    with "../psu_ada_gameengine.gpr";
    ```

## Examples

### Animation Test
![Camera Test](examples/Cloud%20Test.gif)

### Camera Test
![Camera Test](examples/Camera%20Test.gif)