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

with Ada.Finalization;

package Trendy_Terminal.Environments is

    -- RAII-based type to restored the terminal environment on exit.
    type Environment is tagged limited private;

    function Is_Available(Self : Environment) return Boolean;

private

    type Environment is new Ada.Finalization.Limited_Controlled with record
        Initialized : Boolean := False;
    end record;

    overriding
    procedure Initialize (Self : in out Environment);

    overriding
    procedure Finalize (Self: in out Environment);

end Trendy_Terminal.Environments;
