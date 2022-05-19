// qlCpp.cpp - no handling of abolute sections
//{{{  includes
#include <cstdio>
#include <cstdint>

#include <string>
#include <array>
#include <vector>
#include <map>

#include <fstream>

using namespace std;
//}}}
//{{{  debug flags
constexpr bool kOptionDebug = false;
constexpr bool kCmdLineDebug = false;
constexpr bool kObjFileDebug = false;
constexpr bool kPassDebug = false;
//}}}
//{{{  const, enum
// options, easier to use as globals
enum eOption { eChat, eDebug, eMod, eMap, eBell, eXref, eCheck, eBin, eLastOption };
constexpr size_t kNumOptions = eLastOption; // for enum arrays to play nice

// sections
constexpr size_t kNumSections = 16;

// symbol - actually 8 chars for pascal compiler, always at least 2 trailing spaces
constexpr size_t kActualSymbolNameLength = 8;
constexpr size_t kObjSymbolNameLength = 10;

// default section start address, no idea why its this number, VersaDos history ?
constexpr uint32_t kStartBase = 0x400;
//}}}

//{{{
class cSymbol {
public:
  cSymbol (const string& name) : mName(name) {}
  virtual ~cSymbol() = default;

  void addReference (const string& modName) {
    mReferences.push_back (modName);
    }

  string mName;
  string mModName;

  size_t mSection = 0;
  uint32_t mAddress = 0;
  uint32_t mComSize = 0;

  bool mDefined = false;
  bool mComSizeDefined = false;
  bool mFlagged = false;
  bool mUsed = false;
  bool mHist = false;

  vector <string> mReferences;
  };
//}}}
//{{{
class cLinker {
public:
  cLinker() {}
  virtual ~cLinker() = default;

  //{{{
  string getModName() {
    return mModName;
    }
  //}}}
  //{{{
  void setModName (const string& modName) {
    mModName = modName;
    }
  //}}}

  //{{{
  cSymbol* findSymbol (const string& symbolName) {
  // find symbol, return nullptr if not found

    auto it = mSymbolMap.find (symbolName);
    return (it == mSymbolMap.end()) ? nullptr : (*it).second;
    }
  //}}}
  //{{{
  cSymbol* findCreateSymbol (const string& symbolName, bool& symbolFound) {

    cSymbol* symbol = findSymbol (symbolName);

    symbolFound = symbol != nullptr;
    if (symbolFound)
      printf ("symbol already defined %8s\n", symbolName.c_str());
    else {
      symbol = new cSymbol (symbolName);
      mSymbolMap.emplace (symbolName, symbol);
      }

    return symbol;
    }
  //}}}
  //{{{
  void addCommonSymbol (cSymbol* symbol) {
    mCommonSymbols.push_back (symbol);
    }
  //}}}

  //{{{
  void allocCommon() {
  // allocate common section addresses

    for (auto& symbol : mCommonSymbols) {
      mBasePosition = mOptions.getSectionBaseAddress (symbol->mSection);

      symbol->mAddress = mSectBase[symbol->mSection];
      mSectBase[symbol->mSection] = mSectBase[symbol->mSection] + uint32_t(symbol->mComSize);

      if (mSectBase[symbol->mSection] & 1)
        mSectBase[symbol->mSection] = mSectBase[symbol->mSection] + 1;
      }
    }
  //}}}

  //{{{
  void processOptions (const string& line) {
    mOptions.process (line);
    }
  //}}}

  //{{{
  void dumpOptions() {
    mOptions.dump();
    }
  //}}}
  //{{{
  void dumpSymbols() {

    int numUndefined = 0;

    for (auto const& [key, symbol] : mSymbolMap)
      if (symbol->mDefined)
        numUndefined++;

    printf ("%d symbols undefined of %d\n", numUndefined, (int)mSymbolMap.size());
    }
  //}}}
  //{{{
  void dumpSections() {

    for (size_t section = 0; section < 16; section++) {
      uint32_t baseAddress = mOptions.getSectionBaseAddress (section);
      if (baseAddress)
        mBasePosition = baseAddress;

      if (mSectBase[section]) {
        printf ("section:%2d start:%6x size:%6x\n", (int)section, mBasePosition, mSectBase[section]);
        mBaseAddr[section] = mBasePosition;
        mBasePosition = mBasePosition + mSectBase[section];
        }
      }

    printf ("          finish:%6x size:%6x\n", mBasePosition, mBasePosition - mOptions.getSectionBaseAddress(0));
    }
  //}}}

  uint32_t mBasePosition = kStartBase;

  array <uint32_t,16> mSectBase = { 0 };

  array <cSymbol*, 256> mEsdSymbolArray = { nullptr };
  array <uint32_t, 256> mOutAddrArray = { 0 };
  array <uint32_t, 256> mEsdArray = { 0 };

  array <uint32_t, 16> mBaseAddr = { 0 };
  array <uint32_t, 16> mSbase = { 0 };

  uint32_t mCodeStart = 0;
  uint32_t mCodeLen = 0;
  int mTopEsd = 0;

private:
  //{{{
  class cOptions {
  public:
    cOptions() {}
    virtual ~cOptions() = default;

    //{{{
    bool getEnabled (eOption option) {
      return mEnabled[option];
      }
    //}}}
    //{{{
    uint32_t getSectionBaseAddress (size_t section) {
      return mSectionBaseAddress[section];
      }
    //}}}

    //{{{
    void process (const string& line) {

      if (kOptionDebug)
        printf ("processOptions %s\n", line.c_str());

      // parse into individual options, stripping out /
      size_t start = 1;
      size_t found = line.find ('/', start);
      while (found != string::npos)  {
        processToken (line.substr (start, found-start));
        start = found + 1;
        found = line.find ('/', start);
        }

      processToken (line.substr (start, found));
      }
    //}}}
    //{{{
    void dump() {

      printf ("options ");
      for (int optionIndex = eChat; optionIndex <= eBin; optionIndex++)
        if (mEnabled[optionIndex])
          printf ("%s ", kOptionNames [optionIndex].c_str());
      printf ("\n");

      for (int section = 0;  section <= 15; section++)
        if (mSectionBaseAddress[section])
          printf ("  section %d baseAddress:%06x\n", section, mSectionBaseAddress[section]);
      }
    //}}}

  private:
    //{{{
    char getCh() {
      if (mTokenIndex < mToken.size())
        return mToken[mTokenIndex++];
      else // space if no more chars in token, easy to parse
        return ' ';
      }
    //}}}
    //{{{
    uint32_t getHex() {

      uint32_t value = 0;
      while (true) {
        char ch = getCh();
        if ((ch >= '0') && (ch <= '9'))
          value = (value << 4) + (ch - '0');
        else if ((ch >= 'A') && (ch <= 'F'))
          value = (value << 4) + ((ch - 'A') + 10);
        else if ((ch >= 'a') && (ch <= 'f'))
          value = (value << 4) + ((ch - 'a') + 10);
        else
          return value;
        }
      }
    //}}}
    //{{{
    size_t getSection() {

      int value = 0;
      while (true) {
        char ch = getCh();
        if ((ch >= '0') && (ch <= '9'))
          value = (value * 10) + (ch - '0');
        else
          return value;
        }
      }
    //}}}

    //{{{
    void processToken (const string& token) {

      mTokenIndex = 0;
      mToken = token;

      bool found = false;
      for (int optionIndex = eChat; optionIndex <= eBin; optionIndex++)
        if ((token == kOptionNames[optionIndex]) ||(token == kOptionAltNames[optionIndex])) {
          mEnabled[optionIndex] = true;
          found = true;
          break;
          }

      if (!found) {
        // maybe section abase address
        char ch = getCh();
        if (ch == 'o') {
          size_t section = getSection();
          uint32_t address = getHex();
          if ((section >= 0) && (section <= 15))
            mSectionBaseAddress[section] = address;

          if (kOptionDebug)
            printf ("processOption section %s sectionNum:%2d address:%6x\n", token.c_str(), (int)section, address);
          }
        else
          printf ("processOption - unrecognised option %s\n", token.c_str());
        }
      }
    //}}}

    // const
    const array <string, kNumOptions> kOptionNames =    { "chat", "debug", "mod", "map", "bell", "xref", "check", "bin"};
    const array <string, kNumOptions> kOptionAltNames = { "cha",  "deb",   "",    "",    "",     "xrf",  "chk",   ""   };

    // var
    array <bool, kNumOptions> mEnabled = { false };
    array <uint32_t, kNumSections> mSectionBaseAddress = { 0 };

    size_t mTokenIndex = 0;
    string mToken;
    };
  //}}}

  cOptions mOptions;

  string mModName;
  map <string, cSymbol*> mSymbolMap;
  vector <cSymbol*> mCommonSymbols;
  };
//}}}
//{{{
class cObjRecord {
public:
  cObjRecord() {}
  virtual ~cObjRecord() = default;

  enum eRecordType { eNone, eId, eEsd, eObjectText, eEnd };

  // gets
  //{{{
  eRecordType getType() {
    return mType;
    }
  //}}}
  //{{{
  int getDataLeft() {
    return mLength - mBlockIndex - 1;
    }
  //}}}
  //{{{
  uint8_t getByte() {

    if (mBlockIndex >= mLength) {
      printf ("cObjRecord::getByte past end of record data\n");
      return 0;
      }

    return mBlock[mBlockIndex++];
    }
  //}}}
  //{{{
  uint32_t getInt() {

    uint32_t value = 0;
    for (int i = 0; i < 4; i++)
      value = (value << 8) + getByte();

    return value;
    }
  //}}}
  //{{{
  string getSymbolName() {
  // 10 bytes but oonly 8 have ascii chars, trailing spaces pad

    string name;
    name.reserve (kActualSymbolNameLength);

    // read maxSymbolNameLength, only add up to first space in string
    bool terminated = false;
    for (int i = 0; i < kObjSymbolNameLength; i++) {
      char byte = getByte();
      if (byte == ' ')
        terminated = true;

      if (!terminated) {
        if (i < kActualSymbolNameLength)
          name = name + byte;
        else
          printf ("symbolName trailing space problem pos:%d value:%02x\n", i, byte);
        }
      }

    return name;
    }
  //}}}

  // load
  //{{{
  bool loadRoFormat (ifstream& stream) {
  // return true if no more, trying to use EOM shoyld use EOF for conactenated .ro

    mBlockIndex = 0;

    mLength = stream.get();
    uint8_t b = stream.get();

    if ((b > '0') && (b <= '4')) {
      // recognised record type
      mType = (eRecordType)(b - uint8_t('0'));
      if (mType < eEnd)
        for (size_t i = 0; i < mLength-1; i++)
          mBlock[i] = stream.get();

      return mType == eEnd;
      }

    else {
      mType = eNone;
      return true;
      }
    }
  //}}}

  // process
  //{{{
  void processId (cLinker& linker, bool pass1) {

    linker.mTopEsd = 17;
    linker.mEsdArray[0] = 0;  // unused Esd value

    string modName = getSymbolName();
    linker.setModName (modName);

    if (kPassDebug)
      printf ("ModuleId - modName:%s\n", modName.c_str());

    // we need to init these Esd values, in case of zero length sections
    if (!pass1) {
      //if modules then
      //  write (moduleFile, modName, ':');
      //  if files then
      //    write (moduleFile, fileIdString, ':' );
      //  end;

      for (int section = 0; section < 16; section++) {
        linker.mEsdArray[section+1] = linker.mBaseAddr[section] + linker.mSbase[section];
        linker.mEsdSymbolArray[linker.mTopEsd] = nullptr;
        linker.mOutAddrArray[section+1] = linker.mEsdArray[section+1];
        }
      }
    }
  //}}}
  //{{{
  void processEsd (cLinker& linker, bool pass1) {
  // process External Symbol Definition record

    while (getDataLeft() > 0) {
      uint8_t byte = getByte();

      size_t section = byte % 16;
      uint8_t esdType = byte / 16;

      switch (esdType) {
        //{{{
        case  0: { // absolute section
          uint32_t size = getInt();
          uint32_t start = getInt();

          if (kPassDebug)
            printf ("Absolute %08x %08x\n", size, start);

          if (!pass1) {
            linker.mEsdArray[linker.mTopEsd] = start;
            linker.mEsdSymbolArray [linker.mTopEsd] = nullptr;

            linker.mOutAddrArray[linker.mTopEsd] = linker.mEsdArray[linker.mTopEsd];
            linker.mTopEsd = linker.mTopEsd + 1;
            }

          break;
          }
        //}}}
        //{{{
        case  1: { // common section xx
          string symbolName = getSymbolName();
          uint32_t size = getInt();

          if (kPassDebug)
            printf ("common data - section:%2d %8s size:%08x\n", (int)section, symbolName.c_str(), size);

          if (pass1) {
            //{{{  pass1
            bool symbolFound;
            cSymbol* symbol = linker.findCreateSymbol (symbolName, symbolFound);
            symbol->addReference (linker.getModName());

            if (!symbol->mDefined) {
              symbol->mModName = linker.getModName();
              symbol->mSection = section;
              symbol->mComSize = size;
              symbol->mDefined = true;
              linker.addCommonSymbol (symbol);
              }

            else {
              if (size != symbol->mComSize) {
                if (!symbol->mFlagged && !symbol->mComSizeDefined) {
                  //showModName;
                  printf ("Label %s is used double defined\n", symbolName.c_str());
                  printf ("as a common in module %s\n", linker.getModName().c_str());
                  printf (" and as an XDEF in module %s\n", symbol->mModName.c_str());
                  symbol->mFlagged = true;
                  }

                else if (!symbol->mFlagged) {  // and check
                  //showModName;
                  printf ("Common area size clash - common %s\n ", symbolName.c_str());
                  printf ("size in this module is %x bytes", int(size));
                  printf ("size in %s %d bytes\n", symbol->mModName.c_str(), int(symbol->mComSize));
                  symbol->mFlagged = true;
                  }

                if (symbol->mComSizeDefined && (size > symbol->mComSize)) {
                  symbol->mModName = linker.getModName();
                  symbol->mComSize = size;
                  symbol->mComSizeDefined = true;
                  }
                }
              }
            }
            //}}}
          else {
            //{{{  pass2
            }
            //}}}

          break;
          }
        //}}}

        case 2:         // standard relocatable section xx
        //{{{
        case  3: { // short address relocatable section xx
          uint32_t size = getInt();

          if (kPassDebug)
            printf ("section def - section:%2d size:%08x\n", (int)section, size);

          if (pass1) {
            linker.mSectBase[section] = linker.mSectBase[section] + size;
            if (linker.mSectBase[section] & 1)
              linker.mSectBase[section] = linker.mSectBase[section] + 1;
            }

          else {
            linker.mEsdArray[section+1] = linker.mBaseAddr[section] + linker.mSbase[section];
            linker.mEsdSymbolArray [linker.mTopEsd] = nullptr;

            linker.mOutAddrArray[section+1] = linker.mEsdArray[section+1];
            //if modules then
            //  write (moduleFile, ' ', section:2, ':', hex (linker.mEsdArray[section+1], 6, 6), '+', hex (i, 6, 6));

            linker.mSbase[section] = linker.mSbase[section] + size;

            if (linker.mSbase[section] & 1)
              linker.mSbase[section] = linker.mSbase[section] + 1;
            }

          break;
          }
        //}}}

        case 4:         // xDef symbol in relocatble section xx
        //{{{
        case  5: { // xDef symbol in absolute section
          string symbolName = getSymbolName();
          uint32_t address = getInt();

          if (kPassDebug)
            printf ("symbol xdef - section:%2d %8s address:%08x\n", (int)section, symbolName.c_str(), address);

          if (pass1) {
            //{{{  pass1
            bool symbolFound;
            cSymbol* symbol = linker.findCreateSymbol (symbolName, symbolFound);

            // !!! this isnt right yet !!!
            if (symbol->mDefined && !symbol->mFlagged) {
              if (symbol->mHist) {
                //if chat then
                 //  writeln ('redefining ', symbolName);
                }
              else {
                //doubledef(symbol)
                }
              }

            else if (symbolFound) {
              if (symbol->mHist) {
                //if chat then
                //  writeln ('redefining ',symbolName);
                }
              else {
                //numUndefinedSymbols := numUndefinedSymbols - 1;
                }
              }

            symbol->mModName = linker.getModName();
            symbol->mSection = section;
            symbol->mDefined = true;
            symbol->mAddress = address + linker.mSectBase[section];
            }
            //}}}
          else {
            //if usingHistory then
            //  begin { symbol defintion, use to make patches on second pass }
            //  b := findCreate (s, symbol, false); { find it }
            //  if symbol^.resList <> nil then
            //    begin
            //    r := symbol^.resList;
            //    repeat
            //      begin
            //      patch := symbol^.addr + baseaddr[symbol^.section] + r^.offset;
            //      if debugInfo then
            //        writeln ('patching ',hex (r^.addr, 6, 6), ' with ',
            //                             hex (patch-r^.offset, 6, 6), ' + ', hex (r^.offset, 6, 6));
            //      codestart := r^.addr;
            //      codeArray [1] := mvr (mvr (patch));
            //      codeArray [2] := patch;
            //      codelen := 2;
            //      outputData;
            //      r := r^.next;
            //      end until r = nil;
            //    end;
            }
          break;
          }
        //}}}

        case 6:         // xRef symbol to section xx
          //showModName;
          printf ("xref%d\n", int(section));
          // fall through to
        //{{{
        case  7: { // xRef symbol to any section
          string symbolName = getSymbolName();

          if (kPassDebug)
            printf ("symbol xref - section:%2d %8s\n", (int)section, symbolName.c_str());

          if (pass1) {
            // pass1 action
            bool symbolFound;
            cSymbol* symbol = linker.findCreateSymbol (symbolName, symbolFound);
            symbol->addReference (linker.getModName());
            }

          else {
            // pass2 action
            cSymbol* symbol = linker.findSymbol (symbolName);
            if (symbol) {
              linker.mEsdArray[linker.mTopEsd] = symbol->mAddress + linker.mBaseAddr[symbol->mSection];
              linker.mEsdSymbolArray [linker.mTopEsd] = symbol;
              linker.mOutAddrArray[linker.mTopEsd] = linker.mEsdArray[linker.mTopEsd];
              linker.mTopEsd = linker.mTopEsd + 1;
              }
            else
              printf ("internal check failure - lost symbol\n");
            }

          break;
          }
        //}}}

        //{{{
        case  8: { // commandLineAddress in section
          if (kPassDebug)
            printf ("command Line address section\n");

          for (int i = 0; i < 15; i++)
            getByte();

          break;
          }
        //}}}
        //{{{
        case  9: { // commandLineAddress in absolute section
          if (kPassDebug)
            printf ("command Line address absolute section\n");

          for (int i = 0; i < 5; i++)
            getByte();

          break;
          }
        //}}}
        //{{{
        case 10: { // commandLineAddress in common section in section xx
          if (kPassDebug)
            printf ("command Line address common section\n");

          for (int i = 0; i < 15; i++)
            getByte();

          break;
          }
        //}}}

        //{{{
        default:
          if (pass1)
            printf ("unknown EsdType %d\n", esdType);
        //}}}
        }
      }
    }
  //}}}
  //{{{
  void processText (cLinker& linker, bool pass1) {
    //{{{  pascal
    //procedure processText;

    //var
      //bitmap, curresd: integer;

      //{<<<}
      //procedure procbyte;

      //var
        //longwd : boolean;
        //offset, add, i, numesds, offsize : integer;
        //thisesd, w: integer;
        //flag : byte;

        //{<<<}
        //procedure adddata (w:integer);

        //begin
          //duffer := w = %x'4EBA';
          //codelen := codelen + 1;
          //codeArray[codelen] := w;
        //end;
        //{>>>}

      //begin
        //if bitmap >= 0 then
          //begin
          //adddata (mvl (ord (objRecord.block[objRecordBlockIndex])) + ord(objRecord.block[objRecordBlockIndex+1]));
          //objRecordBlockIndex := objRecordBlockIndex + 2;
          //end

        //else
          //begin
          //if duffer then
            //begin
            //showModName;
            //writeln ('Warning - possible assembler foul-up');
            //end;

          //flag := getByte;
          //numesds := flag DIV 32;
          //offsize := flag MOD 8;
          //{ writeln('num esds, ',numesds,'  offset size ',offsize);}
          //longwd := ((flag DIV 8) MOD 2) = 1;

          //add := 0;
          //for i := 1 TO numesds DO
            //begin
            //thisesd := getByte;
            //if thisesd > topESD then
              //begin
              //showModName;
              //writeln (' assembler foul-up.. trying to use an undefined ESD : ' , thisesd);
              //end;

            //if odd(i) then
              //add := add + esdArray[thisesd]
            //else
              //add := add - esdArray[thisesd];
            //end;

          //offset := 0;
          //for i := 1 TO offsize DO offset := mvl(offset) + getByte;
          //CASE offsize of
            //0,4:;
            //1: if offset > 127   then
                //offset := int (uor (uint (offset),%X'FFFFFF00'));
            //2: if offset > 32767 then
                //offset := int (uor (uint (offset),%X'FFFF0000'));
            //end;

          //{writeln('ofFSET ',hex(add,6,6),'+',hex(offset,6,6),'=',hex(add+offset,6,6)); }
          //add := add + offset;
          //if numesds = 0 then
            //begin
            //if odd (offset) then
              //begin
              //showModName;
              //writeln ('odd fix-up offset - assembler error .', offset, curresd);
              //writeln ('>>', hex (codestart, 6, 6));
              //offset := offset + 1;
              //end;

            //if codelen > 0 then
              //outputData;

            //outAddrArray[curresd] := outAddrArray[curresd] + codelen*2 + offset;
            //codelen := 0;
            //codestart := outAddrArray[curresd];
            //end

          //else { numesd <> 0 }
            //begin
            //if NOT longwd then
              //begin
              //if (add > 32767) OR (add < -32768) then
                //begin
                //showModName;
                //writeln ('Long address generated into word location :', hex (add, 8, 8));
                //end;
              //end;

            //if esdSymbolArray [thisesd] <> NIL then { only need named symbols }
              //if modName <> esdSymbolArray [thisesd]^.modName then { outside module }
                //begin
                //if history then
                  //{ address to be resolved LONGWORD only at present}
                  //addRes (esdSymbolArray [thisesd], codestart + codelen*2, offset);

                //if debugInfo then
                  //writeln ('sym ', longwd,
                           //' ', thisesd:2,
                           //' ', esdSymbolArray [thisesd]^.symbolName,
                           //' ', hex (add, 8, 8), ' = ', hex (esdArray[thisesd]),
                           //' + ', hex (offset, 4, 4), ';', hex (offsize, 1, 1),
                           //' at ', hex (codestart + codelen * 2, 8, 8));
                //end;

            //{ generate resolved address }
            //if longwd then
              //adddata (mvr (mvr (add)));
            //adddata (add);
            //end;
          //end;

        //bitmap := bitmap * 2;
      //end;
      //{>>>}

    //begin
      //objRecordBlockIndex := 0;
      //bitmap := getInt;

      //codelen := 0;
      //curresd := getByte;
      //codestart := outAddrArray[curresd];

      //while objRecordBlockIndex < objRecord.length DO
        //procbyte;
      //outputData;

      //{ dont forget convert to bytes}
      //outAddrArray[curresd] := outAddrArray[curresd] + (codelen * 2);
    //end;
    //}}}

    if (!pass1) {
      }
    }
  //}}}

  // dump
  //{{{
  void dump() {

    if (mType == eEnd)
      printf ("end\n");

    else {
      printf ("len:%d type:%d\n", mLength, (int)mType);

      // dump block
      for (int i = 0; i < mLength-1; i++) {
        if ((i % 32) == 0) // indent
          printf ("  %03x  ", i);

        printf ("%02x ", mBlock[i]);
        if ((i % 32) == 31)
          printf ("\n");
        }

      printf ("\n");
      }
    }
  //}}}

private:
  int mLength = 0;
  eRecordType mType = eNone;

  int mBlockIndex = 0;
  array <uint8_t,256> mBlock = {0};
  };
//}}}

//{{{
void processComment (const string& line) {

  if (kCmdLineDebug)
    printf ("processComment %s\n", line.c_str());
  }
//}}}
//{{{
void processInclude (const string& line, vector <string>& objFiles) {
// extract include filename and add its contents to the objFilesfile list

  printf ("processInclude %s not implented\n", line.c_str());
  }
//}}}
//{{{
void processObjFile (const string& line, vector <string>& objFiles) {

  if (kObjFileDebug)
    printf ("processLine %s\n", line.c_str());

  // find any trailing comma
  size_t foundTerminator = line.find (',');
  if (foundTerminator == string::npos) // no comma, find any trailing cr in linux file i/o
    foundTerminator = line.find ('\r');

  // look for extension dot, assumes only one dot, !!! could search backwards !!!
  size_t foundDot = line.find ('.');
  if (foundDot == string::npos) // no dot, use default ext
    objFiles.push_back (line.substr (0, foundTerminator) + ".ro");
  else
    objFiles.push_back (line.substr (0, foundTerminator));
  }
//}}}
//{{{
void processLinker (cLinker& linker, const string& fileName, bool pass1) {

  if (kPassDebug)
    printf ("passFile %s\n", fileName.c_str());

  ifstream objFileStream (fileName, ifstream::in);

  bool eom = false;
  cObjRecord objRecord;
  while (!eom) {
    eom = objRecord.loadRoFormat (objFileStream);

    if (kObjFileDebug && pass1)
      objRecord.dump();

    if (!eom) {
      switch (objRecord.getType()) {
        case cObjRecord::eId:
          objRecord.processId (linker, pass1);
          break;

        case cObjRecord::eEsd:
          objRecord.processEsd (linker, pass1);
          break;

        case cObjRecord::eObjectText:
          objRecord.processText (linker, pass1);
          break;

        case cObjRecord::eEnd:
          break;

        default:
          break;
        }
      }
    }

  objFileStream.close();
  }
//}}}

//{{{
int main (int numArgs, char* args[]) {

  cLinker linker;

  // get command line args
  string cmdFileName;
  for (int i = 1; i < numArgs; i++)
    if (args[i][0] == '/')
      linker.processOptions (args[i]);
    else
      cmdFileName = args[i];

  if (cmdFileName.empty()) {
    //{{{  no .cmd filename - error, exit
    printf ("no .cmd file specified\n");
    return 1;
    }
    //}}}

  printf ("using cmdFileName %s\n", cmdFileName.c_str());

  // get objFiles from .cmd file
  string line;
  vector <string> objFiles;
  ifstream cmdFileStream (cmdFileName + ".cmd", ifstream::in);
  while (getline (cmdFileStream, line))
    if (line[0] == '/')
      linker.processOptions (line);
    else if (line[0] == '!')
      processComment (line);
    else if (line[0] == '#')
      processComment (line);
    else if (line[0] == '@')
      processInclude (line, objFiles);
    else
      processObjFile (line, objFiles);
  cmdFileStream.close();

  linker.dumpOptions();

  // pass 1 - read symbols, accumulate section sizes
  for (auto& objFile : objFiles)
    processLinker (linker, objFile.c_str(), true);
  linker.dumpSymbols();

  // allocate section sizes
  linker.allocCommon();
  linker.dumpSections();

  // pass 2 - resolve addresses and output .bin
  for (auto& objFile : objFiles)
    processLinker (linker, objFile.c_str(), false);

  return 0;
  }
//}}}
