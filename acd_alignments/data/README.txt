Alignments of the Austronesian Comparative Dictionary
2026-11-11

Source
  Blust, Robert, Stephen Trussel, and Alexander D. Smith. The Austronesian
  Comparative Dictionary. CLDF dataset lexibank/acd (v1.2-33-gbc277b3),
  https://github.com/lexibank/acd, licence CC BY 4.0. Please cite the
  dictionary when using these tables.

What is here
  Every reflex of every attested language in the dictionary's cognate sets,
  aligned segment by segment with the reconstruction that heads its set:
  108,046 reflexes of 10,836 reconstructions in 1,012 lects (796 Glottolog
  languages), at the dictionary's nine levels of reconstruction.

    level    reconstructions   reflexes   lects
    PAN            1,767         38,527     824   Proto-Austronesian
    PMP            2,300         29,561     760   Proto-Malayo-Polynesian
    PWMP           3,663         21,105     410   Proto-Western Malayo-Polynesian
    PPH            1,987         10,774     118   Proto-Philippines
    POC              945          6,556     350   Proto-Oceanic
    PCEMP             97            991     240   Proto-Central-Eastern Malayo-Polynesian
    PEMP              33            381     143   Proto-Eastern Malayo-Polynesian
    PCMP              42            145      36   Proto-Central Malayo-Polynesian
    PSHWNG             2              6       3   Proto-South Halmahera-West New Guinea

Files
  acd_reviewed_2026-11-11.csv         the reviewed table: every row with an
                                      alignment the reader confirmed or
                                      supplied (107,231 rows)
  acd_aligned_data.csv                the same rows with the final alignments
                                      alone: without Not_Compared, Remark and
                                      the five review columns
  acd_problem_rows_2026-11-11.csv     the 815 rows still marked as a problem:
                                      728 with no alignment from the reader
                                      (uncertain 668, not cognate 33, other
                                      27), 66 the reader aligned but called
                                      uncertain, and 21 whose rows the reader
                                      left as they were
  acd_not_aligned_2026-11-11.csv      177 forms that could not be read, each
                                      with the reason
  mappings.csv                        how each language's spelling was read: one
                                      row per language (with its Glottocode) and
                                      written letter or letter group, the IPA
                                      segment used for it, whether the value
                                      comes from a rule for that language or
                                      from the general table, and the source
                                      (16,768 rows; the reconstruction letters
                                      of each level are listed first). Made by
                                      running the conversion itself on every
                                      letter found in the language's reflexes.
  The CSV files are UTF-8 with a byte order mark, so that Excel shows the IPA.

How the tables were made
  1. Each reconstruction and reflex was converted from the dictionary's
     spelling to IPA segments. The spelling of each language was read as the
     dictionary the ACD cites for it describes it, where that dictionary or a
     grammar could be read (about fifty were; see review/orthography).
  2. Each reflex was aligned with its reconstruction by a trained aligner.
  3. Every alignment was then read by a large language model acting as a
     reviewer with knowledge of Austronesian historical phonology,
     which confirmed it, corrected it, or flagged a problem. No person has read
     the alignments. Two independent readings of the same 145 rows agreed on
     144.

  Review     rows      meaning
  confirmed  99,458    the automatic alignment stands
  corrected   4,460    same segments, realigned by the reader
  segments corrected
              3,265    the reader changed the segments compared, and the table
                       shows the reader's rows: the wrong part of the reflex
                       had been kept (1,318), an affix of the reconstruction
                       was mis-marked (935), a letter was read as the wrong
                       sound (839), the wrong word of a phrase was taken (173).
                       Problem gives the kind.
  resolved on second reading
                 48    rows the first reader left open, which two further
                       readers, working independently, aligned in the same way.
                       747 open rows were read again like this; where the two
                       did not both give the same alignment the row was left as
                       it was.
  problem       815    left open (in the file of problem rows, not in the
                       reviewed table): see Files.

Columns
  Row_ID             level : cognate set : form ID of the ACD
  Level              the level of the reconstruction (table above)
  ACD_Set            the ACD's cognate set ID
  Reconstruction     the reconstruction as the ACD writes it
  Gloss              the ACD's gloss of the reconstruction
  Language, Glottocode, ACD_Group
  Reflex             the reflex as the ACD writes it, whole
  Reflex_Gloss       the ACD's gloss of the reflex
  Ancestor_Aligned   the reconstruction's segments in IPA, one cell each,
                     separated by spaces; "-" where the reflex has a segment
                     that continues nothing in the reconstruction; [x y] an
                     affix, infix or reduplicant of the reconstruction; (x) a
                     segment the ACD writes as optional or of unknown value
  Reflex_Aligned     the reflex's segments under them; "-" where the ancestor's
                     segment has no reflex
  Not_Compared       ancestor segments left out of the comparison for this pair
  Remark             "word N (...) of the phrase ..." where the reflex is cited
                     as a phrase and one word was taken (4,560 rows); "loan"
                     where the ACD marks the reflex as a loan (248 rows)
  Review             confirmed, corrected, segments corrected, resolved on
                     second reading, or problem
  Problem            the kind of problem
  Review_Note        the reader's note, in linguistic notation
  Automatic_Ancestor_Aligned, Automatic_Reflex_Aligned
                     the automatic alignment, where the reader replaced it

Conventions worth knowing
  - The ACD lists 16 forms twice in one set; each is given once here.
  - Reconstructions: *e is schwa (but a mid vowel e in Proto-Oceanic); *q is
    written q, *R as a uvular trill, *j as a voiced palatal stop, *z and
    Proto-Oceanic *j as a voiced palatal affricate, *c as a voiceless palatal;
    Proto-Austronesian *C is ts, *N a palatal lateral, *S is s and *s a palatal
    fricative; *D a retroflex stop (Blust 2013: 561). These are Blust's assumed
    values.
  - Voiced stops are written plain throughout, although those of Proto-Oceanic
    and of Fijian, Wayan and Nggela were or are prenasalised: the spelling of
    the dictionary does not show prenasalisation, and sources were read for
    few of the Oceanic languages.
  - A long vowel is one cell (aː). A doubled vowel letter is read as one long
    vowel in the languages whose dictionaries write length that way (Puluwat,
    Chuukese, Woleaian, Kapingamarangi, Nukuoro, Sa'a) and, outside the
    Philippines, wherever one of the two copies has no segment of the
    reconstruction to continue (Sio baani from *paNid). Two like vowels that
    each continue a segment stay two cells.
  - Stress marks of the source dictionaries are removed. Where a reflex has
    more morphemes than the reconstruction, only the part that continues the
    reconstruction is kept; Reflex shows the whole form.
  - Where the ACD heads a set with two reconstructions (*tenzeg, te(n)dek) the
    first is used, unless the reader found that the reflex continues the other,
    in which case the row shows the other form in Ancestor_Aligned and its
    Review_Note says so.
  - Spelling values taken from secondary sources because the cited dictionary
    could not be read: Marshallese, Rennellese, Rotuman, Chuukese. The letter e
    is read as schwa by its correspondences alone, with no source read, in
    Balinese, Iban, Sasak, Dairi-Pakpak, Rembong, Simalur, Ngadha, Kenyah,
    Kavalan, Pazeh, Itbayaten, Ibatan, Kankanaey, Agutaynen, Ayta Abellan,
    Palawan Batak, Kalamian Tagbanwa and Ivatan.
