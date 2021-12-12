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

    procedure Reset (Self : in out Completion_Set) is
    begin
        Self.Lines.Clear;
        Self.Index := 0;
    end Reset;

    procedure Fill
        (Self  : in out Completion_Set;
         Lines : Trendy_Terminal.Lines.Line_Vectors.Vector)
    is
        use type Ada.Containers.Count_Type;
    begin
        Self.Lines := Lines;
        Self.Index := (if Lines.Length > 0 then 1 else 0);
    end Fill;

    procedure Set_Index (Self : in out Completion_Set; Index : Integer) is
    begin
        if Index <= 0 then
            Self.Index := Length (Self);
        else
            Self.Index := Index mod Length (Self);
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

    function Length (Self : Completion_Set) return Integer is
    begin
        return 0;
    end Length;

    function Is_Valid (Self : Completion_Set) return Boolean is
    begin
        return Self.Index /= 0;
    end Is_Valid;

end Trendy_Terminal.Completions;
