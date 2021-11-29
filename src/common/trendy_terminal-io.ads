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

    procedure New_Line (Num_Lines : Positive := 1);
    procedure Set_Col (Column : Positive);

    type Format_Function is access function (L : Lines.Line) return Lines.Line;

    -- Attempts to complete a line.
    --
    -- Completion_Index is the N'th attempted completion of the line.
    -- Shift-tab should decrease the Completion_Index,
    -- tab should increase the Completion_Index.
    type Completion_Function is access function (L : Lines.Line)
        return Lines.Line_Vectors.Vector;

    -- Line editing
    --
    -- A description of the elements involved to modify a line of text.
    type Line_Editor is interface;
    function Get_Line (Editor : in out Line_Editor'Class) return String;

    function Format   (E : in out Line_Editor; L : Lines.Line) return Lines.Line is abstract;
    function Complete (E : in out Line_Editor; L : Lines.Line) return Lines.Line_Vectors.Vector is abstract;

    type Stateless_Line_Editor is new Line_Editor with record
        Format_Fn     : Format_Function;
        Completion_Fn : Completion_Function;
    end record;

    overriding
    function Format (E : in out Stateless_Line_Editor; L : Lines.Line) return Lines.Line;

    overriding
    function Complete (E : in out Stateless_Line_Editor; L : Lines.Line) return Lines.Line_Vectors.Vector;

    -- Helper to implicitly use a Stateless_Line_Editor
    function Get_Line (Format_Fn     : Format_Function := null;
                       Completion_Fn : Completion_Function := null) return String;

end Trendy_Terminal.IO;
