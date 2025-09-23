# Spectrox - Prelude to Neobyte

This repo contains all the source code and assets of "Prelude to Neobyte", a TIC-80 demo by Spectrox.

__Notes:__

- Needs TIC-80 PRO to directly load .lua files.
- The main file uses the `require` function to include additional files and assets. Adjust the `package.path` accordingly.
- Run the `build.sh` script to pack all files into one single file. `require` and `package.path` will be removed in the resulting file.
- After running the build script, load the generated .lua file into TIC-80 PRO and save as a .tic file with `save demo.tic`.

__Tools:__

- [File to C style array converter](https://notisrac.github.io/FileToCArray/) to convert assets.
- Gradients were created with [Gradient Blaster](https://gradient-blaster.grahambates.com) by Gigabates/DESiRE.

---
Olympian / Spectrox, 2025
