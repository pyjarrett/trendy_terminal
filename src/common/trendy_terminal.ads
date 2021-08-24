with Ada.Strings.Unbounded;

package Trendy_Terminal is
    package ASU renames Ada.Strings.Unbounded;

    function Init return Boolean;
    procedure Shutdown;

    type Input_Setting is (Echo, Line_Input);
    type Output_Setting is (Escape_Sequences);

    procedure Set (Setting : Input_Setting; Enabled : Boolean);
    procedure Set (Setting : Output_Setting; Enabled : Boolean);

    procedure Write_Terminal(C : Character);
    procedure Write_Terminal(S : String);
    procedure Write_Terminal_Line(S : String);

    function Get_Input return String;
    function Get_Line return String;

    -- A debug version of Get_Line for learning how to write an appropriate
    -- interface and callback system for Get_Line.
    function Debug_Get_Line return String;

    type Cursor_Position is record
        Row : Integer;
        Col : Integer;
    end record;

    function Get_Cursor_Position return Cursor_Position;

    type Key is (Key_Up, Key_Left, Key_Right, Key_Down,
                  Key_Ctrl_Up, Key_Ctrl_Left, Key_Ctrl_Right, Key_Ctrl_Down,
                  Key_F1, Key_F2, Key_F3, Key_F4,
                  Key_F5, Key_F6, Key_F7, Key_F8,
                  Key_F9, Key_F10, Key_F11, Key_F12,
                  Key_Backspace, Key_Pause, Key_Escape,
                  Key_Home, Key_End,
                  Key_Insert, Key_Delete,
                  Key_Page_Up, Key_Page_Down);
end Trendy_Terminal;
