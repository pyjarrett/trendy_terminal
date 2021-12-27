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

with Ada.Characters.Latin_1;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;

package Trendy_Terminal.Maps is
    package ASU renames Ada.Strings.Unbounded;
    package Characters renames Ada.Characters.Latin_1;

    CSI : constant String := Characters.ESC & "[";

    type Key is (Key_Up, Key_Left, Key_Right, Key_Down,
                  Key_F1, Key_F2, Key_F3, Key_F4,
                  Key_F5, Key_F6, Key_F7, Key_F8,
                  Key_F9, Key_F10, Key_F11, Key_F12,
                  Key_Backspace, Key_Pause, Key_Escape,
                  Key_Home, Key_End,
                  Key_Insert, Key_Delete,
                  Key_Page_Up, Key_Page_Down,
                  Key_Tab,

                  -- Keys with modifiers.
                  Key_Shift_Tab,
                  Key_Ctrl_Up, Key_Ctrl_Left, Key_Ctrl_Right, Key_Ctrl_Down,

                  Key_Ctrl_C, Key_Ctrl_D
                  );

    function Sequence_For (K : Key) return String;
    function Is_Key (Sequence : String) return Boolean;
    function Key_For (Sequence : String) return Key
        with Pre => Is_Key (Sequence);

end Trendy_Terminal.Maps;
