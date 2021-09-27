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

with Trendy_Terminal.Platform;

package body Trendy_Terminal.Environments is

    function Is_Available(Self : Environment) return Boolean is (Self.Initialized);

    overriding
    procedure Initialize (Self : in out Environment) is
    begin
        Self.Initialized := Trendy_Terminal.Platform.Init;
    end Initialize;

    overriding
    procedure Finalize(Self : in out Environment) is
    begin
        if Self.Initialized then
            Trendy_Terminal.Platform.Shutdown;
        end if;
    end Finalize;

end Trendy_Terminal.Environments;
