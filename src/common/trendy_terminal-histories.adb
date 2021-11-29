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

with Ada.Strings.Fixed;

package body Trendy_Terminal.Histories is

    procedure Add (H : in out History; L : Lines.Line) is
    begin
        H.Entries.Append (L);
    end Add;

    procedure Set_Max_Entries (H : in out History; Count : Positive) is
    begin
        H.Max_Entries := Count;
    end Set_Max_Entries;

    function Num_Entries (H : History) return Natural is
    begin
        return Natural (H.Entries.Length);
    end Num_Entries;

    function Get_Entry (H : History; Index : Positive) return Lines.Line is
    begin
        return H.Entries (Index);
    end Get_Entry;

    function Completions_Matching (H : History; Incomplete : String) return Lines.Line_Vectors.Vector is
        V : Lines.Line_Vectors.Vector;
    begin
        for L : Lines.Line of H.Entries loop
            declare
                Line_Contents : String := Lines.Current (L);
            begin
                if Ada.Strings.Fixed.Index (Line_Contents, Incomplete) = 0 then
                    V.Append (L);
                end if;
            end;
        end loop;
        return V;
    end Completions_Matching;

end Trendy_Terminal.Histories;
