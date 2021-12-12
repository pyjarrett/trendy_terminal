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

with Trendy_Terminal.VT100;

package body Trendy_Terminal.Lines is
    function Length(Self : in Line) return Natural is (Current(Self)'Length);

    procedure Move_Cursor (Self : in out Line; Direction : Cursor_Direction) is
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

    function Make (Contents : ASU.Unbounded_String; Index : Positive) return Line is
    begin
        return Line'(Contents => Contents, Cursor => Index);
    end Make;

    function Make (S : String; Index : Positive) return Line is
    begin
        return Result : Line do
            Result.Contents := ASU.To_Unbounded_String (S);
            Result.Cursor := Index;
        end return;
    end Make;

    function Make (S : String) return Line is
    begin
        return Make (S, S'Length + 1);
    end Make;

    procedure Set (Self : in out Line; S : String; Index : Positive) is
    begin
        Self.Contents := ASU.To_Unbounded_String (S);
        Self.Cursor := Index;
    end Set;

    function Get_Cursor_Index (Self : in Line) return Positive is
    begin
        return Self.Cursor;
    end Get_Cursor_Index;

    procedure Set_Cursor_Index (Self : in out Line; Cursor_Index : Positive) is
    begin
        Self.Cursor := Cursor_Index;
    end Set_Cursor_Index;

    procedure Insert (Self : in out Line; S : String) is
    begin
        ASU.Insert(Self.Contents, Self.Cursor, S);
        Self.Cursor := Self.Cursor + S'Length;
    end Insert;

    procedure Backspace (Self : in out Line) is
    begin
        if Self.Cursor = 1 then
            return;
        end if;
        ASU.Delete(Self.Contents, Self.Cursor - 1, Self.Cursor - 1);
        Move_Cursor(Self, Left);
    end Backspace;

    procedure Delete (Self : in out Line) is
    begin
        if ASU.Length (Self.Contents) > 0 and then Self.Cursor <= ASU.Length(Self.Contents) then
            ASU.Delete (Self.Contents, Self.Cursor, Self.Cursor);

            if Self.Cursor > ASU.Length (Self.Contents) + 1 then
                Move_Cursor (Self, Left);
            end if;
        end if;
    end Delete;

    procedure Clear (Self : in out Line) is
    begin
        Self.Cursor := 1;
        Self.Contents := ASU.Null_Unbounded_String;
    end Clear;

    function Current (Self : Line) return String is (ASU.To_String(Self.Contents));
end Trendy_Terminal.Lines;
