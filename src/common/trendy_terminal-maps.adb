with Ada.Characters.Latin_1;
with Trendy_Terminal.VT100;

package body Trendy_Terminal.Maps is
    use all type ASU.Unbounded_String;
    function "+"(S : String) return ASU.Unbounded_String renames ASU.To_Unbounded_String;

    function Make_Key_Map return Key_Maps.Map is
        KM : Key_Maps.Map;
    begin
        KM.Insert(VT100.CSI & (+"A"), Key_Up);
        KM.Insert(VT100.CSI & (+"B"), Key_Down);
        KM.Insert(VT100.CSI & (+"C"), Key_Right);
        KM.Insert(VT100.CSI & (+"D"), Key_Left);
        KM.Insert(VT100.CSI & (+"H"), Key_Home);
        KM.Insert(VT100.CSI & (+"F"), Key_End);

        KM.Insert(VT100.CSI & (+"1;5A"), Key_Ctrl_Up);
        KM.Insert(VT100.CSI & (+"1;5B"), Key_Ctrl_Down);
        KM.Insert(VT100.CSI & (+"1;5C"), Key_Ctrl_Right);
        KM.Insert(VT100.CSI & (+"1;5D"), Key_Ctrl_Left);

        KM.Insert(Ada.Characters.Latin_1.DEL & ASU.Null_Unbounded_String, Key_Backspace);
        KM.Insert(Ada.Characters.Latin_1.SUB & ASU.Null_Unbounded_String, Key_Pause);

        KM.Insert(VT100.CSI & (+"2~"), Key_Insert);
        KM.Insert(VT100.CSI & (+"3~"), Key_Delete);
        KM.Insert(VT100.CSI & (+"5~"), Key_Page_Up);
        KM.Insert(VT100.CSI & (+"6~"), Key_Page_Down);

        KM.Insert(Ada.Characters.Latin_1.ESC & (+"OP"), Key_F1);
        KM.Insert(Ada.Characters.Latin_1.ESC & (+"OQ"), Key_F2);
        KM.Insert(Ada.Characters.Latin_1.ESC & (+"OR"), Key_F3);
        KM.Insert(Ada.Characters.Latin_1.ESC & (+"OS"), Key_F4);

        KM.Insert(VT100.CSI & (+"15~"), Key_F5);
        KM.Insert(VT100.CSI & (+"17~"), Key_F6);
        KM.Insert(VT100.CSI & (+"18~"), Key_F7);
        KM.Insert(VT100.CSI & (+"19~"), Key_F8);

        KM.Insert(VT100.CSI & (+"20~"), Key_F9);
        KM.Insert(VT100.CSI & (+"21~"), Key_F10);
        KM.Insert(VT100.CSI & (+"23~"), Key_F11);
        KM.Insert(VT100.CSI & (+"24~"), Key_F12);

        KM.Insert(Ada.Characters.Latin_1.HT & (+""), Key_Tab);

        return KM;
    end Make_Key_Map;

    function Make_Key_Lookup_Map return Inverse_Key_Maps.Map is
        KM : Inverse_Key_Maps.Map;
    begin
        KM.Insert(Key_Up,    VT100.CSI & (+"A"));
        KM.Insert(Key_Down,  VT100.CSI & (+"B"));
        KM.Insert(Key_Right, VT100.CSI & (+"C"));
        KM.Insert(Key_Left,  VT100.CSI & (+"D"));
        KM.Insert(Key_Home,  VT100.CSI & (+"H"));
        KM.Insert(Key_End,   VT100.CSI & (+"F"));

        KM.Insert(Key_Ctrl_Up,    VT100.CSI & (+"1;5A"));
        KM.Insert(Key_Ctrl_Down,  VT100.CSI & (+"1;5B"));
        KM.Insert(Key_Ctrl_Right, VT100.CSI & (+"1;5C"));
        KM.Insert(Key_Ctrl_Left,  VT100.CSI & (+"1;5D"));

        KM.Insert(Key_Backspace, Ada.Characters.Latin_1.DEL & (+""));
        KM.Insert(Key_Pause,     Ada.Characters.Latin_1.SUB & (+""));

        KM.Insert(Key_Insert,    VT100.CSI & (+"2~"));
        KM.Insert(Key_Delete,    VT100.CSI & (+"3~"));
        KM.Insert(Key_Page_Up,   VT100.CSI & (+"5~"));
        KM.Insert(Key_Page_Down, VT100.CSI & (+"6~"));

        KM.Insert(Key_F1, Ada.Characters.Latin_1.ESC & (+"OP"));
        KM.Insert(Key_F2, Ada.Characters.Latin_1.ESC & (+"OQ"));
        KM.Insert(Key_F3, Ada.Characters.Latin_1.ESC & (+"OR"));
        KM.Insert(Key_F4, Ada.Characters.Latin_1.ESC & (+"OS"));

        KM.Insert(Key_F5, VT100.CSI & (+"15~"));
        KM.Insert(Key_F6, VT100.CSI & (+"17~"));
        KM.Insert(Key_F7, VT100.CSI & (+"18~"));
        KM.Insert(Key_F8, VT100.CSI & (+"19~"));

        KM.Insert(Key_F9,  VT100.CSI & (+"20~"));
        KM.Insert(Key_F10, VT100.CSI & (+"21~"));
        KM.Insert(Key_F11, VT100.CSI & (+"23~"));
        KM.Insert(Key_F12, VT100.CSI & (+"24~"));

        KM.Insert(Key_Tab, Ada.Characters.Latin_1.HT & (+""));

        return KM;
    end Make_Key_Lookup_Map;
end Trendy_Terminal.Maps;
