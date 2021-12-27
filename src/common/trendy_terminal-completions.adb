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

with Ada.Containers;

package body Trendy_Terminal.Completions is

    procedure Clear (Self : in out Completion_Set) is
    begin
        Self.Lines.Clear;
        Self.Index := 1;
    end Clear;

    function Is_Empty (Self : Completion_Set) return Boolean is
        use type Ada.Containers.Count_Type;
    begin
        return Self.Lines.Length = 0;
    end Is_Empty;

    procedure Fill
        (Self  : in out Completion_Set;
         Lines : Trendy_Terminal.Lines.Line_Vectors.Vector)
    is
        use type Ada.Containers.Count_Type;
    begin
        Self.Lines := Lines;
        Self.Index := 1;
    end Fill;

    procedure Set_Index (Self : in out Completion_Set; Index : Integer) is
    begin
        Self.Index := Index;
        if Self.Index <= 0 then
            Self.Index := Length (Self);
        else
            Self.Index := Self.Index mod Length (Self);
            if Self.Index = 0 then
                Self.Index := Length (Self);
            end if;
        end if;
    end Set_Index;

    procedure Move_Forward (Self : in out Completion_Set) is
    begin
        Set_Index (Self, Self.Index + 1);
    end Move_Forward;

    procedure Move_Backward (Self : in out Completion_Set) is
    begin
        Set_Index (Self, Self.Index - 1);
    end Move_Backward;

    function Get_Current (Self : in out Completion_Set) return String is
    begin
        return Trendy_Terminal.Lines.Current (Self.Lines (Self.Index));
    end Get_Current;

    function Get_Index (Self : in out Completion_Set) return Integer is
    begin
        return Self.Index;
    end Get_Index;

    function Length (Self : Completion_Set) return Integer is
    begin
        return Integer (Self.Lines.Length);
    end Length;

end Trendy_Terminal.Completions;
