# Changelog

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
but adds emojis.

Types of changes:

- ✅ `Added` for new features.
- 🔄 `Changed` for changes in existing functionality.
- ⚠️ `Deprecated` for soon-to-be removed features.
- ❌ `Removed` for now removed features.
- 🛠️ `Fixed` for any bug fixes.
- 💥💥💥 `Security` in case of vulnerabilities. Triple 💥 for emphasis.

# [[Unreleased]]

- 🛠️ Fixed [CRASH #1](https://github.com/pyjarrett/trendy_terminal/issues/1) when input preexists prior to `Get_Input`.
- ✅ Added `Put` procedures like those in `Ada.Text_IO`.
- ✅ Added formatting callbacks to `Get_Line`.
- ✅ Added `Beginning_Of_Line` to move cursor.
- 🔄 Changed `Clear_Line` to only clear line and not move the cursor.
- ✅ Added calling of tab completion function.
- ✅ Added movement of cursor with HOME and END.
- ✅ Added `Environment` type for RAII restoration of the environment.
- ⚠️ Deprecated direct usage of `Init` and `Shutdown`.
- ❌ Removed `Trendy_Terminal` as the primary package everything is in.
- 🔄 Changed location of `Init`, `Shutdown` and input/output settings to `Trendy_Terminal.Platform`.

