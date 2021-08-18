with Trendy_Terminal.VT100;

package body Trendy_Terminal.Input is

    procedure Move_Cursor (Self : in out Line; Direction : Cursor_Direction) is
    begin
        case Direction is
            when Left =>
                if Self.Cursor > 1 then
                    Self.Cursor := Self.Cursor - 1;
                    VT100.Cursor_Left;
                end if;
            when Right =>
                if Self.Cursor < ASU.Length (Self.Contents) then
                    Self.Cursor := Self.Cursor + 1;
                    VT100.Cursor_Right;
                end if;
        end case;
    end Move_Cursor;

    procedure Insert (Self : in out Line; S : String) is
    begin
        ASU.Insert(Self.Contents, Self.Cursor, S);
        Self.Cursor := Self.Cursor + S'Length;
    end Insert;

    procedure Clear (Self : in out Line) is
    begin
        Self.Cursor := 1;
        Self.Contents := ASU.Null_Unbounded_String;
    end Clear;

    function Current (Self : Line) return String is (ASU.To_String(Self.Contents));

end Trendy_Terminal.Input;
