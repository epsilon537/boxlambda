# The User Interface

A Canvas based REPL and Text Editor.

A Canvas is a character grid of 76 columns and a variable number of rows.

In memory a Canvas is a consecutive memory range of extended-ASCII encoded bytes. Offset 0 corresponds with the top-left character in the Canvas, the last offset correspond to the bottom-right character in the grid.

Multiple Canvases can exist in memory. They can be loaded from and saved to the file system. When saving, trailing spaces on each 80-wchar line are replaced with a newline. This allows canvas files to be read as regular text files by traditional text editors.

Note that an in-memory Canvas does not contain any newline or carriage return characters. All lines are 80 characters long.

A Canvas is created with an initial number of lines, 59 by default. As text is being written on the Canvas, the Canvas may grow up to 999 lines, tracking the number of lines in use.

A Canvas has an *Input* and an *Output* cursor. The *Input Cursor* points to where the next input character will be stored. The *Output Cursor* points to where the next output character (e.g. by Forth's *type*) will be stored.

Cursor navigation, text insertion/overwrite and selection follows vi conventions.

Commands/keyboard shortcuts are provided to:
- allows the user to switch between input and output cursor navigation.
- peg the output cursor to the input cursor.
- trim to the line containing the active cursor.

In Command Mode, when the return key is pressed and no selection is active, the line under the input cursor is evaluated. If a selection is active, the selected text is evaluated.

The display is configured as a 60x80 character display. The display is divided into 1 or more *ViewPorts*. A ViewPort provides a view into a Canvas.
Each Viewport is as wide as the screen, but its height and vertical offset on the display may vary. The bottom row of a ViewPort contains a *Mode Line*. The leftmost 4 columns show a 3-digit line number followed by a space. This arrangement leaves columns for the display of Canvas lines.

Initially, the display consists of a single ViewPort covering the entire display.

Commands/keyboard shortcuts are provided to:
- split a ViewPort into a top and bottom ViewPort.
- close a top or bottom ViewPort, in which case the remaining ViewPort takes over the entire display.

A ViewPort points to a starting line of a Canvas and displays as many lines starting from that line as the ViewPort holds. If the ViewPort extends past the end of the Canvas, the Canvas grows empty lines to accommodate the ViewPort.

Multiple ViewPorts can point to the same Canvas, or to different Canvases.

