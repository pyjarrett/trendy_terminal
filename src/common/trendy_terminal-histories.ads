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
with Trendy_Terminal.String_Vectors;

-- Stores user line history.
--
-- The goal here is to have a history type which can be loaded from file,
-- but also completed like a line.
--
-- This gets used as a form of completion for line editing.
package Trendy_Terminal.Histories is

    -- A record of the history of user inputs.
    type History is private;

    type History_Access is access all History;

    -- Adds a line to the history.  This makes it the most recent line in the
    -- history.
    procedure Add (H : in out History; Input : String);

    -- Some histories only store a limited number of entries.
    procedure Set_Max_Entries(H : in out History; Count : Positive);

    function Num_Entries (H : History) return Natural;

    -- History starting with 1 being the least recent, with the most recent
    -- command having a higher index.
    function Get_Entry (H : History; Index : Positive) return String
        with Pre => Index < Positive (Num_Entries (H));

    -- Returns a list of completions which could match the given line.
    function Completions_Matching (H : History; Incomplete : String) return Lines.Line_Vectors.Vector;

private

    type History is record
        Entries     : String_Vectors.Vector;
        Max_Entries : Positive := Positive'Last;
    end record;

end Trendy_Terminal.Histories;
