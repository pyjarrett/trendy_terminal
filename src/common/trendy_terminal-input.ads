package Trendy_Terminal.Input is

    type Line is private;

    type Cursor_Direction is (Left, Right);
    procedure Move_Cursor (Self : in out Line; Direction : Cursor_Direction);

    procedure Insert (Self : in out Line; S : String);

    procedure Clear (Self : in out Line);
    function Current (Self : Line) return String;

private

    type Line is record
        Contents : ASU.Unbounded_String := ASU.Null_Unbounded_String;
        Cursor   : Positive := 1;
    end record;

end Trendy_Terminal.Input;
