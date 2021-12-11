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

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Trendy_Terminal.Lines.Line_Vectors;
with Trendy_Terminal.Platform;
with Trendy_Terminal.VT100;

package Trendy_Terminal.IO is
    package ASU renames Ada.Strings.Unbounded;

    -- These are platform-specific terminal read/write functions to avoid
    -- messing with Ada standard library internals such as current column.
    -- This avoids spurious formatting and other implementation quirks of
    -- those libraries.
    procedure Put (C : Character) renames Trendy_Terminal.Platform.Put;
    procedure Put (S : String) renames Trendy_Terminal.Platform.Put;
    procedure Put (S : ASU.Unbounded_String);
    procedure Put_Line (S : String);
    procedure Put_Line (S : ASU.Unbounded_String);

    procedure Rewrite_Line (Pos : VT100.Cursor_Position; S : String);

    procedure New_Line (Num_Lines : Positive := 1);
    procedure Set_Col (Column : Positive);

end Trendy_Terminal.IO;
