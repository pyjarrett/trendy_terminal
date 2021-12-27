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

with Trendy_Terminal.Lines.Line_Vectors;

package Trendy_Terminal.Completions is

    -- A group of possible completions that need to be searched.
    type Completion_Set is private;

    procedure Clear (Self : in out Completion_Set);
    function Is_Empty (Self : Completion_Set) return Boolean;

    -- Sets the data used by the completion set.
    procedure Fill
        (Self  : in out Completion_Set;
         Lines : Trendy_Terminal.Lines.Line_Vectors.Vector);

    procedure Set_Index (Self : in out Completion_Set; Index : Integer)
        with Pre => not Is_Empty (Self);

    procedure Move_Forward (Self : in out Completion_Set)
        with Pre => not Is_Empty (Self);

    procedure Move_Backward (Self : in out Completion_Set)
        with Pre => not Is_Empty (Self);

    function Get_Current (Self : in out Completion_Set) return String
        with Pre => not Is_Empty (Self);

    function Get_Index (Self : in out Completion_Set) return Integer
        with Pre => not Is_Empty (Self);

    function Length (Self : Completion_Set) return Integer;

private

    type Completion_Set is record
        Lines : Trendy_Terminal.Lines.Line_Vectors.Vector;
        Index : Integer;
    end record;

end Trendy_Terminal.Completions;
