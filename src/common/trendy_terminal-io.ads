with Ada.Strings.Unbounded;
with Trendy_Terminal.Platform;

package Trendy_Terminal.IO is
    package ASU renames Ada.Strings.Unbounded;

    -- These are platform-specific terminal read/write functions to avoid
    -- messing with Ada standard library internals such as current column.
    -- This avoids spurious formatting and other implementation quirks of
    -- those libraries.
    procedure Put (C : Character) renames Trendy_Terminal.Platform.Put;
    procedure Put (S : String) renames Trendy_Terminal.Platform.Put;
    procedure Put (S : ASU.Unbounded_String);
    procedure Put_Line (S : String);
    procedure Put_Line (S : ASU.Unbounded_String);

    procedure Clear_Line;
    procedure New_Line (Num_Lines : Positive);
    procedure Set_Col (Column : Positive);

    type Format_Function is access function (S : String) return String;
    type Completion_Function is access function (S: String) return String;

    function Get_Line (Format_Fn     : Format_Function := null;
                       Completion_Fn : Completion_Function := null) return String;

    function Get_Cursor_Position return Cursor_Position;

end Trendy_Terminal.IO;
