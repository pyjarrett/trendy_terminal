package body Trendy_Terminal.Maps is
    use all type ASU.Unbounded_String;
    function "+"(S : String) return ASU.Unbounded_String renames ASU.To_Unbounded_String;

    function Make_Key_Map return Key_Maps.Map is
        KM : Key_Maps.Map;
    begin
        KM.Insert(CSI & (+"A"), Key_Up);
        KM.Insert(CSI & (+"B"), Key_Down);
        KM.Insert(CSI & (+"C"), Key_Right);
        KM.Insert(CSI & (+"D"), Key_Left);
        KM.Insert(CSI & (+"H"), Key_Home);
        KM.Insert(CSI & (+"F"), Key_End);

        KM.Insert(CSI & (+"1;5A"), Key_Ctrl_Up);
        KM.Insert(CSI & (+"1;5B"), Key_Ctrl_Down);
        KM.Insert(CSI & (+"1;5C"), Key_Ctrl_Right);
        KM.Insert(CSI & (+"1;5D"), Key_Ctrl_Left);

        KM.Insert(Characters.DEL & ASU.Null_Unbounded_String, Key_Backspace);
        KM.Insert(Characters.SUB & ASU.Null_Unbounded_String, Key_Pause);

        KM.Insert(CSI & (+"2~"), Key_Insert);
        KM.Insert(CSI & (+"3~"), Key_Delete);
        KM.Insert(CSI & (+"5~"), Key_Page_Up);
        KM.Insert(CSI & (+"6~"), Key_Page_Down);

        KM.Insert(Characters.ESC & (+"OP"), Key_F1);
        KM.Insert(Characters.ESC & (+"OQ"), Key_F2);
        KM.Insert(Characters.ESC & (+"OR"), Key_F3);
        KM.Insert(Characters.ESC & (+"OS"), Key_F4);

        KM.Insert(CSI & (+"15~"), Key_F5);
        KM.Insert(CSI & (+"17~"), Key_F6);
        KM.Insert(CSI & (+"18~"), Key_F7);
        KM.Insert(CSI & (+"19~"), Key_F8);

        KM.Insert(CSI & (+"20~"), Key_F9);
        KM.Insert(CSI & (+"21~"), Key_F10);
        KM.Insert(CSI & (+"23~"), Key_F11);
        KM.Insert(CSI & (+"24~"), Key_F12);

        KM.Insert(Characters.HT & (+""), Key_Tab);
        KM.Insert(CSI & (+"Z"), Key_Shift_Tab);

        return KM;
    end Make_Key_Map;

    function Make_Key_Lookup_Map return Inverse_Key_Maps.Map is
        KM : Inverse_Key_Maps.Map;
    begin
        -- https://vt100.net/docs/vt510-rm/DECFNK.html
        --
        -- 0,1  none
        -- 2    Shift
        -- 3    Alt
        -- 4    Alt + Shift
        -- 5    Control
        -- 6    Control + Shift
        -- 7    Alt + Control
        -- 8    Alt + Control + Shift

        KM.Insert(Key_Up,    CSI & (+"A"));
        KM.Insert(Key_Down,  CSI & (+"B"));
        KM.Insert(Key_Right, CSI & (+"C"));
        KM.Insert(Key_Left,  CSI & (+"D"));
        KM.Insert(Key_Home,  CSI & (+"H"));
        KM.Insert(Key_End,   CSI & (+"F"));

        KM.Insert(Key_Ctrl_Up,    CSI & (+"1;5A"));
        KM.Insert(Key_Ctrl_Down,  CSI & (+"1;5B"));
        KM.Insert(Key_Ctrl_Right, CSI & (+"1;5C"));
        KM.Insert(Key_Ctrl_Left,  CSI & (+"1;5D"));

        KM.Insert(Key_Backspace, Characters.DEL & (+""));
        KM.Insert(Key_Pause,     Characters.SUB & (+""));

        KM.Insert(Key_Insert,    CSI & (+"2~"));
        KM.Insert(Key_Delete,    CSI & (+"3~"));
        KM.Insert(Key_Page_Up,   CSI & (+"5~"));
        KM.Insert(Key_Page_Down, CSI & (+"6~"));

        KM.Insert(Key_F1, Characters.ESC & (+"OP"));
        KM.Insert(Key_F2, Characters.ESC & (+"OQ"));
        KM.Insert(Key_F3, Characters.ESC & (+"OR"));
        KM.Insert(Key_F4, Characters.ESC & (+"OS"));

        KM.Insert(Key_F5, CSI & (+"15~"));
        KM.Insert(Key_F6, CSI & (+"17~"));
        KM.Insert(Key_F7, CSI & (+"18~"));
        KM.Insert(Key_F8, CSI & (+"19~"));

        KM.Insert(Key_F9,  CSI & (+"20~"));
        KM.Insert(Key_F10, CSI & (+"21~"));
        KM.Insert(Key_F11, CSI & (+"23~"));
        KM.Insert(Key_F12, CSI & (+"24~"));

        KM.Insert(Key_Tab, Characters.HT & (+""));
        KM.Insert(Key_Shift_Tab, CSI & (+"Z"));

        return KM;
    end Make_Key_Lookup_Map;
end Trendy_Terminal.Maps;
