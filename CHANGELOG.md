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

# [[0.0.5]]

- ✅ `Added` experimental support for Mac.
- 🛠️ `Fixed` some problems with Linux support.

# [[0.0.4]]

- 🛠️ Fix jumping cursor when line editing.
- ✅ Adds command history scrollback using arrow keys.

# [[0.0.3]]

- 🛠️ Fix single character printing on Windows.

# [[0.0.2]]

- ✅ Added `Put` procedures like those in `Ada.Text_IO`.
- ✅ Added formatting callbacks to `Get_Line`.
- ✅ Added `Beginning_Of_Line` to move cursor.
- ✅ Added calling of tab completion function.
- ✅ Added movement of cursor with HOME and END.
- ✅ Added `Environment` type for RAII restoration of the environment.
- 🔄 Changed `Clear_Line` to only clear line and not move the cursor.
- 🔄 Changed location of `Init`, `Shutdown` and input/output settings to `Trendy_Terminal.Platform`.
- ⚠️ Deprecated direct usage of `Init` and `Shutdown`.
- ❌ Removed `Trendy_Terminal` as the primary package everything is in.

