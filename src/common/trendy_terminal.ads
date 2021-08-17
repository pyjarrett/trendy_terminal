package Trendy_Terminal is

    function Init return Boolean;
    procedure Shutdown;

    type Input_Setting is (Echo, Line_Input);
    type Output_Setting is (Escape_Sequences);

    procedure Set (Setting : Input_Setting; Enabled : Boolean);
    procedure Set (Setting : Output_Setting; Enabled : Boolean);

    procedure Write_Terminal(C : Character);
    procedure Write_Terminal(S : String);
    procedure Write_Terminal_Line(S : String);

    -- Gets an entire input line from one keypress.  E.g. all the characters
    -- received for a controlling keypress, such as an arrow key.
    function Get_Input return String;
    function Get_Line return String;

end Trendy_Terminal;
