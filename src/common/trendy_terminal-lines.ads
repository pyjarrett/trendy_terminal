-------------------------------------------------------------------------------
-- Copyright 2021, The Trendy Terminal Developers (see AUTHORS file)

-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at

--     http://www.apache.org/licenses/LICENSE-2.0

-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded;

-- Ties together Lines to associated VT100 commands.
package Trendy_Terminal.Lines is
    package ASU renames Ada.Strings.Unbounded;

    -- The number of individual cursor positions in a string.
    -- TODO: Support UTF-8
    function Num_Cursor_Positions (S : String) return Natural is (S'Length);

    -- A generic line to Lines text using a single cursor.
    --
    -- The cursor position is defined as being the position at which the new
    -- Lines will appear.  For an empty line, the only valid cursor position
    -- is 1.  For other lines, with a line range of [1,n], appropriate cursor
    -- positions will be [1, n+1], 1 being "new character will be added at
    -- index 1", and n+1 meaning "appending and increasing the length of the
    -- Lines line".
    --
    -- Sample:  sample
    -- Length:  6
    -- Indices: 123456
    -- Cursor:    ^
    -- Valid cursor range: [1, 7]
    --
    type Line is private
        with Type_Invariant => Get_Cursor_Index (Line) in 1 .. Length (Line) + 1;

    type Cursor_Direction is (Left, Right);
    function Length(Self : in Line) return Natural;
    procedure Move_Cursor (Self : in out Line; Direction : Cursor_Direction);

    function Make (Contents : ASU.Unbounded_String; Index : Positive) return Line;
    function Make (S : String; Index : Positive) return Line;
    function Make (S : String) return Line;
    procedure Set (Self : in out Line; S : String; Index : Positive);

    function Get_Cursor_Index (Self : in Line) return Positive;
    procedure Set_Cursor_Index (Self : in out Line; Cursor_Index : Positive);

    procedure Insert (Self : in out Line; S : String)
        with Pre => S'Length >= 0,
            Post => Length(Self'Old) + S'Length = Length(Self)
            and then Get_Cursor_Index (Self'Old) + Num_Cursor_Positions (S) = Get_Cursor_Index(Self);

    procedure Backspace (Self : in out Line)
        with Post => Length(Self'Old) = 0
            or else Get_Cursor_Index(Self'Old) = 1
            or else (Length(Self'Old) = Length(Self) + 1
                and then Get_Cursor_Index(Self'Old) - 1 = Get_Cursor_Index(Self));

    -- Deletes a characters after the cursor position, shifting all text
    -- afterwards to the left.  Deleting does not modify the cursor position.
    -- Nothing happens if cursor is after the last character in the Line.
    procedure Delete (Self : in out Line)
        with Post => Length (Self'Old) = 0
            or else Get_Cursor_Index (Self'Old) = Length (Self) + 1
            or else (Length(Self'Old) = Length(Self) + 1
                and then Get_Cursor_Index (Self'Old) = Get_Cursor_Index (Self));

    procedure Clear (Self : in out Line)
        with Post => Length (Self) = 0
            and then Get_Cursor_Index (Self) = 1;

    function Current (Self : Line) return String;

    overriding
    function "="(Left, Right : Line) return Boolean is (
        Get_Cursor_Index (Left) = Get_Cursor_Index (Right)
        and then Current (Left) = Current (Right));

private

    type Line is record
        Contents : ASU.Unbounded_String := ASU.Null_Unbounded_String;
        Cursor   : Positive := 1;
    end record;

end Trendy_Terminal.Lines;
