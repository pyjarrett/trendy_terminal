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

    type Completion_Set is record
        Lines : Trendy_Terminal.Lines.Line_Vectors.Vector;
        Index : Integer;
    end record;

    procedure Reset (Self : in out Completion_Set);
    procedure Fill (Self : in out Completion_Set; Lines : Trendy_Terminal.Lines.Line_Vectors.Vector);
    procedure Set_Index (Self : in out Completion_Set; Index : Integer);

    procedure Move_Forward (Self : in out Completion_Set);
    procedure Move_Backward (Self : in out Completion_Set);

    function Get_Current (Self : in out Completion_Set) return String
        with Pre => Self.Index > 0 and then Is_Valid (Self);

    function Is_Valid (Self : Completion_Set) return Boolean;
    function Length (Self : Completion_Set) return Integer;

end Trendy_Terminal.Completions;
