# Perceptron Pente

_Version 1.0.2+20250611  ([Version Release Notes](#ReleaseNotes))_ 

**Perceptron Pente** is an open source implementation of the **[Pente](https://en.wikipedia.org/wiki/Pente)** board game that uses simplified neural network inspired artificial intelligence pattern matching.

The game can be played by two human players, by a human against the computer, or by two computer players.

The pattern matching appproach is loosely based on the **[Single Layer Perceptron](https://gist.github.com/udayzee05/f878f7807b2c201ad400fc3818a59c2d)** developed during early research into neural networks.

For each computer player (White vs. Black), a set of Perceptron pattern match definitions are used to assign a numerical weight to each open board position. During each computer player move, the computer places its piece on the board position with the highest weight.

After each game ends, the program performs partial _mutations_ on some of the players' Perceptron sets. The type of mutations differ for the winning vs. the losing player. One aspect of the mutation algorithm increases the weighting preference for the most used Perceptron after a winning game.

The goal is to slowly accumulate beneficial mutations that improve the strength of one or both computer players over time. The program supports unattended sessions of multiple _Auto Play_ games between the two computer players.

To support this long-term strategy across multiple sessions, the program allows the current state of the players' Perceptron sets to be save to a JSON text file. The program can reload a previously saved JSON file to continue _evolving_ the computer players' Perceptron sets.

The program allows the human player to play against either of the computer players. This allows independent assessment of the computer player strengths. It may also provide _forcing feedback_ to influence the computer's Perceptron set mutations.

## About the Software

The software is a self-contained executable program, written in **[Free Pascal](https://www.freepascal.org/)**, that runs on Microsoft Windows or Ubuntu Linux (and presumably other Linux distributions).

(No separate run-time environment is required to run the program.)
The **[Lazarus Integrated Development Environment](https://www.lazarus-ide.org/)** was used to develop the program.
(Both Free Pascal and the Lazarus IDE are free open-source software products.) 

## Downloading and Running the Program

### Microsoft Windows

You can run the Pascal Pente program on Microsoft Windows as follows:

- Download the **PerceptronPente.exe** binary executable file from the **bin** sub-folder from this GitHub.com page.

- To uninstall the program, simply delete the **PerceptronPente.exe** file.

### Ubuntu Linux

You can run the Pascal Pente program on Ubuntu Linux (and presumably other Linux distributions) as follows:

- Download the **PerceptronPente** binary executable file (with no file extension) from the **bin** sub-folder from this GitHub.com page.

- Ensure the **PerceptronPente** file has the executable permission.  From a Files window, right-click the file, select Properties, and use the Permissions tab to enable the Execute permission.  To do this in a Terminal window, use the following command:
  
    chmod +x PerceptronPente

- To uninstall the program, simply delete the **PerceptronPente** binary executable file.

### Running the Program

In a file folder viewer, double-click the downloaded copy of **PerceptronPente.exe** (on Windows) or **PerceptronPente** (on Linux) to start the program.

When the program starts it displays the **Perceptron Pente** form.

Here is an image of the Perceptron Pente form during a game

![PerceptronPente Form](img/PerceptronPente-Form.png?raw=true "PerceptronPente Form")

The Form contains these elements:

- The 9x9 Game Board. Left-click on any cell to make a manual move for White. Right-click on any cell to make a manual move for Black.
- The corresponding 9x9 grid showing cell move weights base on Perceptron matching scores.
- A text label showing the most recent game winner.
- Click the **Play White** button to have the computer make a move for White based on Perceptron pattern matching. The Wins and Losses statistics for White appear next to this button.
- Click the **Play Black** button to have the computer make a move for Black based on Perceptron pattern matching. The Wins and Losses statistics for Black appear next to this button.
- Click tne **New Game** button to clear the Game Board to start a new game.
- Click the **Auto Play** button to have the computer play both players agains each other until one wins.  The _spin edit_ control determines how many successive Auto Play games to play. Type a number, or use the _up_ or _down_ arrow to set the value.
- Click the **Write Perceptrons to File** button to save the current Perceptron sets for both players to a JSON text file. A file save dialog will appear.
- Click the **Read Perceptrons from File** button to load both players' Perceptron sets from a previously saved JSON text file. (The **dat** sub-folder contains a _pre-evolved JSON text file.)
- Click the **Randomize Perceptrons** button to generate new random Perceptron sets for both players.

When the program first starts, the **Randomize Perceptrons** feature is automatically executed. You may wish to reload the player Perceptron sets from a previously saved JSON text file.

Notes on **Auto Play**:

- While an Auto Play session is in progress. the **Auto Play** button changes into a **Pause** button.
- Click the **Pause** button to suspend the Auto Play session. The button returns to **Auto Play**.
- You can finish the paused game using manual moves in the Game Board, or by clicking the **Play White** or **Play Black** buttons, to continue the Auto Play session with a new game.

## Source code compilation notes

Download the **Lazarus IDE**, including **Free Pascal**, from  here:

- **<https://www.lazarus-ide.org/index.php?page=downloads>**

After installing the **Lazarus IDE**, clone this GitHub repository to your local disk.
Then double-click on the **src\PerceptronPente.lpr** project file to open it in **Lazarus**. 

_**Note:**_ Using the debugger in the **Lazarus IDE** on Windows 10 _**might**_ require the following configuration adjustment:

- **[Lazarus - Windows - Debugger crashing on OpenDialog](https://www.tweaking4all.com/forum/delphi-lazarus-free-pascal/lazarus-windows-debugger-crashing-on-opendialog/)**

When **Lazarus** includes debugging information the executable file is relatively large.
When ready to create a release executable, the file size can be significantly reduced by selecting the menu item **Project | Project Options ...** and navigating to the **Compile Options | Debugging** tab in the resulting dialog window.
Clear the check-mark from the **Generate info for the debugger** option and then click the **OK** button.
Then rebuild the executable using the **Run | Build** menu item (or using the shortcut key-stroke _**Shift-F9**_).

<a name="ReleaseNotes"></a>

## Release Notes

### Version 1.0.1

- Avoid main thread hang by using background thread for **Auto Play**.
- Allow Pause of **Auto Play**.
- Include _pre-evolved_ Perceptrons JSON text file in the **dat** folder.
- Include the Windows and Linux executable files in the **bin** folder.

### Version 1.0.0

- The initial version of the software.
