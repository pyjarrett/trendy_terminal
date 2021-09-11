-- Ties together input to associated VT100 commands.
package Trendy_Terminal.Input is

    -- The number of individual cursor positions in a string.
    -- TODO: Support UTF-8
    function Num_Cursor_Positions (S : String) return Natural is (S'Length);

    -- A generic line to input text using a single cursor.
    --
    -- The cursor position is defined as being the position at which the new
    -- input will appear.  For an empty line, the only valid cursor position
    -- is 1.  For other lines, with a line range of [1,n], appropriate cursor
    -- positions will be [1, n+1], 1 being "new character will be added at
    -- index 1", and n+1 meaning "appending and increasing the length of the
    -- input line".
    --
    -- Sample:  sample
    -- Length:  6
    -- Indices: 123456
    -- Cursor:    ^
    -- Valid cursor range: [1, 7]
    --
    type Line_Input is private
        with Type_Invariant => Cursor_Index (Line_Input) in 1 .. Length (Line_Input) + 1;

    type Cursor_Direction is (Left, Right);
    function Length(Self : in Line_Input) return Natural;
    procedure Move_Cursor (Self : in out Line_Input; Direction : Cursor_Direction);
    function Cursor_Index (Self : in Line_Input) return Positive;

    procedure Insert (Self : in out Line_Input; S : String)
        with Pre => S'Length >= 0,
            Post => Length(Self'Old) + S'Length = Length(Self)
            and then Cursor_Index (Self'Old) + Num_Cursor_Positions (S) = Cursor_Index(Self);

    procedure Backspace (Self : in out Line_Input)
        with Post => Length(Self'Old) = 0
            or else Cursor_Index(Self'Old) = 1
            or else (Length(Self'Old) = Length(Self) + 1
                and then Cursor_Index(Self'Old) - 1 = Cursor_Index(Self));

    -- Deletes a characters after the cursor position, shifting all text
    -- afterwards to the left.  Deleting does not modify the cursor position.
    -- Nothing happens if cursor is after the last character in the Line_Input.
    procedure Delete (Self : in out Line_Input)
        with Post => Length (Self'Old) = 0
            or else Cursor_Index (Self'Old) = Length (Self) + 1
            or else (Length(Self'Old) = Length(Self) + 1
                and then Cursor_Index (Self'Old) = Cursor_Index (Self));

    procedure Clear (Self : in out Line_Input)
        with Post => Length (Self) = 0
            and then Cursor_Index (Self) = 1;

    function Current (Self : Line_Input) return String;

private

    type Line_Input is record
        Contents : ASU.Unbounded_String := ASU.Null_Unbounded_String;
        Cursor   : Positive := 1;
    end record;

end Trendy_Terminal.Input;
