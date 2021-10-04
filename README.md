# matchpatch-amiga

Source code and program file for the 1990 Amiga game MatchPatch. A detailed
article on the game's history and analyzing its function can be found here:

<https://tetracorp.github.io/amiga/matchpatch.html>

## File listing

* __MatchPatch__:  Standalone main game executable. Uncompressed.
* __MatchPatch.DOC__:  Readme from Amiga Computing #33 (Feb 1991) coverdisk.
* __MatchPatch.S.asm__:  MatchPatch v1.0 source code (12-10-92) by SD.
* __MatchPatch.S.pp__:  Compressed version of the MatchPatch source as originally distributed. Identical to the other source file except for the compression.
* __MatchPatch.DOC.info, MatchPatch.S.info, MatchPatch.info, .info__:  Icon files.
* __MatchPatchDocs.txt__:  An excerpt from Amiga Computing #33 documenting the
  game and its cheat codes.
* __MatchPatch_mod__:  A hex-edited cheat version of Match Patch. It gives you 99 lives.
* __maps.md__:  Maps extracted from the game.
* __README.md__:  This file.

Historic files have been committed with the original date, usually the filedate
from the Amiga Computing coverdisk. Commits of this sort can be done like so:

> git add MatchPatch.S.asm
>
> GIT_AUTHOR_DATE="Fri 12 Oct 12:00:00 GMT 1990" GIT_COMMITTER_DATE="Fri 12 Oct 12:00:00 GMT 1990" git commit -m "Adding MatchPatch.S.asm at historic date"

## License

MatchPatch is Copyright &copy; 1991 Amiga Computing.

"MatchPatch was written in assembly language (Devpac 2) by Stephen Winstanley.
It may be offered for download on bulletin boards provided it is made available
as a free download." (Amiga Computing #33, Feb 1991, p.32)
