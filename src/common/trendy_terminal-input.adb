with Trendy_Terminal.VT100;

package body Trendy_Terminal.Input is
    function Length(Self : in Line_Input) return Natural is (Current(Self)'Length);

    procedure Move_Cursor (Self : in out Line_Input; Direction : Cursor_Direction) is
    begin
        case Direction is
            when Left =>
                if Self.Cursor > 1 then
                    Self.Cursor := Self.Cursor - 1;
                    VT100.Cursor_Left;
                end if;
            when Right =>
                if Self.Cursor <= ASU.Length (Self.Contents) then

                    Self.Cursor := Self.Cursor + 1;
                    VT100.Cursor_Right;
                end if;
        end case;
    end Move_Cursor;

    function Get_Cursor_Index (Self : in Line_Input) return Positive is
    begin
        return Self.Cursor;
    end Get_Cursor_Index;

    procedure Set_Cursor_Index (Self : in out Line_Input; Cursor_Index : Positive) is
    begin
        Self.Cursor := Cursor_Index;
    end Set_Cursor_Index;

    procedure Insert (Self : in out Line_Input; S : String) is
    begin
        ASU.Insert(Self.Contents, Self.Cursor, S);
        Self.Cursor := Self.Cursor + S'Length;
    end Insert;

    procedure Backspace (Self : in out Line_Input) is
    begin
        if Self.Cursor = 1 then
            return;
        end if;
        ASU.Delete(Self.Contents, Self.Cursor - 1, Self.Cursor - 1);
        Move_Cursor(Self, Left);
    end Backspace;

    procedure Delete (Self : in out Line_Input) is
    begin
        if ASU.Length (Self.Contents) > 0 and then Self.Cursor <= ASU.Length(Self.Contents) then
            ASU.Delete (Self.Contents, Self.Cursor, Self.Cursor);

            if Self.Cursor > ASU.Length (Self.Contents) + 1 then
                Move_Cursor (Self, Left);
            end if;
        end if;
    end Delete;

    procedure Clear (Self : in out Line_Input) is
    begin
        Self.Cursor := 1;
        Self.Contents := ASU.Null_Unbounded_String;
    end Clear;

    function Current (Self : Line_Input) return String is (ASU.To_String(Self.Contents));
end Trendy_Terminal.Input;
