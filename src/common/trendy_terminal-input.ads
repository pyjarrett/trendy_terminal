with Ada.Containers.Ordered_Maps;

-- Ties together input to associated VT100 commands.
package Trendy_Terminal.Input is

    type Line is private;

    type Cursor_Direction is (Left, Right);
    procedure Move_Cursor (Self : in out Line; Direction : Cursor_Direction);
    function Cursor_Index (Self : in out Line) return Positive;

    procedure Insert (Self : in out Line; S : String);
    procedure Backspace (Self : in out Line);

    procedure Clear (Self : in out Line);
    function Current (Self : Line) return String;

    ---------------------------------------------------------------------------
    -- Input Mapping
    ---------------------------------------------------------------------------

    package Key_Maps is new Ada.Containers.Ordered_Maps (Key_Type => ASU.Unbounded_String,
                                                         Element_Type => Key,
                                                         "<" => ASU."<",
                                                         "=" => "=");

    package Inverse_Key_Maps is new Ada.Containers.Ordered_Maps (Key_Type => Key,
                                                         Element_Type => ASU.Unbounded_String,
                                                         "<" => "<",
                                                         "=" => ASU."=");

    function Make_Key_Map return Key_Maps.Map;
    function Make_Key_Lookup_Map return Inverse_Key_Maps.Map;

private

    type Line is record
        Contents : ASU.Unbounded_String := ASU.Null_Unbounded_String;
        Cursor   : Positive := 1;
    end record;

end Trendy_Terminal.Input;
