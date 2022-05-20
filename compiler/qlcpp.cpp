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
constexpr bool kObjectFileDebug = false;
constexpr bool kPassDebug = false;
constexpr bool kOutDebug = true;
//}}}
//{{{  const, enum
// options, easier to use as globals
enum eOption { eChat, eDebug, eMod, eMap, eBell, eXref, eCheck, eBin, eLastOption };
constexpr size_t kNumOptions = eLastOption; // for enum arrays to play nice

// sections
constexpr size_t kNumSections = 16;

// symbol - actually 8 chars for pascal compiler, always at least 2 trailing spaces
constexpr size_t kActualSymbolNameLength = 8;
constexpr size_t kObjectSymbolNameLength = 10;

// default section start address, no idea why its this number, VersaDos history ?
constexpr uint32_t kStartBase = 0x400;

constexpr uint8_t esc = 0x1B;
//}}}
constexpr bool out = true;
constexpr bool bin = false;
constexpr bool download = false;
constexpr bool escape = false;

//{{{
class cSymbol {
public:
  cSymbol (const string& name) : mName(name) {}
  virtual ~cSymbol() = default;

  //{{{
  void addReference (const string& modName) {
    mReferences.push_back (modName);
    }
  //}}}

  string mName;
  string mModName;

  uint8_t mSection = 0;
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

    for (uint8_t section = 0; section < 16; section++) {
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
  int mTopEsd = 0;

  string mModName;

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
    uint32_t getSectionBaseAddress (uint8_t section) {
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
    uint8_t getSection() {

      uint8_t value = 0;
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

  map <string, cSymbol*> mSymbolMap;
  vector <cSymbol*> mCommonSymbols;
  };
//}}}
//{{{
class cObjectRecord {
public:
  cObjectRecord() : mOutputMaxSize (bin ? 512 : 16) {}
  virtual ~cObjectRecord() = default;

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
  uint8_t getUint8() {
  // get 1 bytes to form uint8_t result

    if (mBlockIndex >= mLength) {
      printf ("cObjectRecord::getUint8 past end of record data\n");
      return 0;
      }

    return mBlock[mBlockIndex++];
    }
  //}}}
  //{{{
  uint32_t getUint32() {
  // get 4 bytes to form uint32_t result

    uint32_t value = 0;
    for (int i = 1; i <= 4; i++)
      value = (value << 8) + getUint8();

    return value;
    }
  //}}}
  //{{{
  string getSymbolName() {
  // 10 bytes but only 8 have ascii chars, trailing spaces pad
  // force upperCase

    string name;
    name.reserve (kActualSymbolNameLength);

    // read maxSymbolNameLength, but only append to name up to first space in string
    bool spaceTerminated = false;
    for (int i = 0; i < kObjectSymbolNameLength; i++) {
      char byte = getUint8();
      if (byte == ' ')
        spaceTerminated = true;

      if (!spaceTerminated) {
        if (i < kActualSymbolNameLength)
          name = name + char(toupper (byte));
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
      uint8_t byte = getUint8();
      uint8_t section = byte & 0x0F;
      uint8_t esdType = byte >> 4;

      switch (esdType) {
        //{{{
        case  0: { // absolute section
          uint32_t size = getUint32();
          uint32_t start = getUint32();

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
          uint32_t size = getUint32();

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
          uint32_t size = getUint32();

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
          uint32_t address = getUint32();

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
            //      linker->mCodeStart := r^.addr;
            //      codeArray [1] := mvr (mvr (patch));
            //      codeArray [2] := patch;
            //      mCodeLen := 2;
            //      outputCode;
            //      r := r^.next;
            //      end until r = nil;
            //    end;
            }
          break;
          }
        //}}}

        case 6:         // xRef symbol to section xx - unexpected
          printf ("xref to section:%d\n", int(section));
          [[fallthrough]];
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
        case  8: { // commandLineAddress in section - unexpected
          if (kPassDebug)
            printf ("command Line address section\n");

          for (int i = 0; i < 15; i++)
            getUint8();

          break;
          }
        //}}}
        //{{{
        case  9: { // commandLineAddress in absolute section - unexpected
          if (kPassDebug)
            printf ("command Line address absolute section\n");

          for (int i = 0; i < 5; i++)
            getUint8();

          break;
          }
        //}}}
        //{{{
        case 10: { // commandLineAddress in common section in section xx - unexpected
          if (kPassDebug)
            printf ("command Line address common section\n");

          for (int i = 0; i < 15; i++)
            getUint8();

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
  void processText (cLinker& linker, ofstream& stream) {
  // process text record, to out stream, pass 2 only

    uint32_t bitmap = getUint32();
    uint8_t curEsd = getUint8();
    linker.mCodeStart = linker.mOutAddrArray[curEsd];

    if (kOutDebug)
      printf ("output bitmap:%08x curEsd:%d\n", bitmap, curEsd);

    mCodeLen = 0;
    uint8_t thisEsd = 0;
    while (getDataLeft() > 0) {
      if (bitmap & 0x80000000) {
        //{{{  get rest of record
        uint8_t byte = getUint8();
        uint8_t numEsds = byte >> 5;
        uint8_t offsetSize = byte & 0x07;

        if (kOutDebug)
          printf ("numEsds:%d offsetSize:%d\n", numEsds, offsetSize);

        bool longWd = offsetSize & 0x08;

        uint32_t add = 0;
        for (int i = 1; i <= numEsds; i++) {
          thisEsd = getUint8();
          if (kOutDebug)
            printf ("thisEsd:%d topEsd:%d\n", thisEsd, linker.mTopEsd);
          if (thisEsd > linker.mTopEsd) {
            //{{{  foulup
            // showModName;
            printf ("trying to use an undefined ESD %d of %d\n", thisEsd, linker.mTopEsd);
            }
            //}}}

          if (i & 0x1)
            add = add + linker.mEsdArray[thisEsd];
          else
            add = add - linker.mEsdArray[thisEsd];
          }

        uint32_t offset = 0;
        for (int i = 1; i <= offsetSize; i++)
          offset = (offset << 8) + getUint8();

        switch (offsetSize) {
          case 1: if (offset > 127)
                    offset = offset | 0xFFFFFF00;
                  break;
          case 2: if (offset > 32767)
                    offset = offset | 0xFFFF0000;
                   break;
          default:;
          }
        // printf ("ofFSET %08x + %08x = %08x\n", add, offset, add+offset);

        add = add + offset;
        if (numEsds == 0) {
          //{{{  numEsds == 0
          if (offset & 0x01) {
            //showModName;
            printf ("odd fix-up offset - assembler error %6x %d, %6x\n", offset, curEsd, linker.mCodeStart);
            offset = offset + 1;
            }

          if (mCodeLen > 0)
            outputCode (stream);
          linker.mOutAddrArray[curEsd] = linker.mOutAddrArray[curEsd] + mCodeLen*2 + offset;

          mCodeLen = 0;
          mCodeStart = linker.mOutAddrArray[curEsd];
          }
          //}}}
        else {
          if (!longWd) {
            if ((add > 32767) || (add < -32768)) {
              //{{{  foulup
              //showModName;
              printf ("Long address generated into word location:%6x\n", add);
              }
              //}}}
            }

          if (linker.mEsdSymbolArray [thisEsd] != nullptr) {
            // only need named symbols
            if (linker.mModName != linker.mEsdSymbolArray [thisEsd]->mModName) {
              //{{{  outside module
              //if history then
              //  { address to be resolved LONGWORD only at present}
              //  addRes (esdSymbolArray [thisesd], linker.mCodeStart + mCodeLen*2, offset);

              //if debugInfo then
              //  writeln ('sym ', longwd,
              //           ' ', thisesd:2,
              //           ' ', esdSymbolArray [thisesd]^.symbolName,
              //           ' ', hex (add, 8, 8), ' = ', hex (esdArray[thisesd]),
              //           ' + ', hex (offset, 4, 4), ';', hex (offsetSize, 1, 1),
              //           ' at ', hex (linker.mCodeStart + mCodeLen * 2, 8, 8));
              }
              //}}}

            // generate resolved address
            if (longWd)
              mCodeArray[++mCodeLen] = add >> 16;
            mCodeArray[++mCodeLen] = add;
            }
          }
        }
        //}}}
      else {
        //{{{  add word of code to codeArray
        uint16_t value = getUint8() << 8;
        value |= getUint8();

        mCodeArray[++mCodeLen] = value;
        }
        //}}}
      bitmap = bitmap << 1;
      }

    outputCode (stream);

    // convert to bytes}
    linker.mOutAddrArray[curEsd] = linker.mOutAddrArray[curEsd] + (mCodeLen * 2);
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
  //{{{
  string toHex (uint8_t byte) {
    string result;

    uint8_t digit = byte >> 4;
    result += digit > 9 ? 'a' + digit - 10 : '0' + digit;

    digit = byte & 0x0F;
    result += digit > 9 ? 'a' + digit - 10 : '0' + digit;

    return  result;
    }
  //}}}
  //{{{
  void sendBin (ofstream& stream, uint8_t b) {

    if ((b == esc) && escape)
      stream.put (b);

    stream.put (b);
    }
  //}}}
  //{{{
  void wbyte (ofstream& stream, uint8_t b) {

    if (bin) {
      sendBin (stream, b);
      outputChecksum ^= b;
      }
    else {
      string s = toHex (b);
      stream.write (s.c_str(), s.length());
      outputChecksum += b;
      }
    }
  //}}}
  //{{{
  void outputPacket (ofstream& stream, uint32_t pos, uint32_t len) {
  // output single packet of codeArray

    mPos = pos;
    mLen = len;
    uint32_t codeStart = mCodeStart + mPos*2;
    uint32_t packetLen = mLen*2 + 4; // this happens to be right for both

    if (bin) {
      //{{{
      sendBin (stream, esc);
      sendBin (stream, 0);
      wbyte (stream, 1);
      wbyte (stream, packetLen >> 8);
      wbyte (stream, packetLen & 0xFF);
      wbyte (stream, codeStart >> 24);
      wbyte (stream, codeStart >> 16);
      wbyte (stream, codeStart >> 8);
      wbyte (stream, codeStart & 0xFF);

      //b,cstart,i:integer;
      //outputChecksum := 0;

      //for (int i = 1; i <= len; i++) {
        //b = int (uand (%x'FFFF', uint (codeArray[i+pos]))) DIV 256;
        //if (b = esc) AND (escape = true) then
          //begin
          //binBlock[binBlockIndex] := b;
          //binBlockIndex := binBlockIndex + 1;
          //if binBlockIndex > 511 then
            //begin
            //if out then write
              //(binaryFile, binBlock);
            //if download then
              //write (downloadTargetFile, binBlock);
            //binBlockIndex := 0;
            //end;
          //end;

        //binBlock[binBlockIndex] := b;
        //binBlockIndex := binBlockIndex + 1;
        //if binBlockIndex > 511 then
          //begin
          //if out then
            //write (binaryFile, binBlock);
          //if download then
            //write (downloadTargetFile, binBlock);
          //binBlockIndex := 0;
          //end;

        //outputChecksum := ord(uxor(uint(b),uint(outputChecksum)));

        //b := codeArray[i+pos] MOD 256;
        //if (b = esc) AND (escape = true) then
          //begin
          //binBlock[binBlockIndex] := b;
          //binBlockIndex := binBlockIndex + 1;
          //if binBlockIndex > 511 then
            //begin
            //if out then
              //write(binaryFile, binBlock);
            //if download then
              //write (downloadTargetFile, binBlock);
            //binBlockIndex := 0;
            //end;
          //end;

        //binBlock[binBlockIndex] := b;
        //binBlockIndex := binBlockIndex + 1;
        //if binBlockIndex > 511 then
          //begin
          //if out then
            //write(binaryFile, binBlock);
          //if download then
            //write(downloadTargetFile, binBlock);
          //binBlockIndex := 0;
          //end;
        //outputChecksum := ord (uxor (uint(b), uint (outputChecksum)));
        //end

      sendBin (stream, outputChecksum);
      }
      //}}}
    else {
      // packet start
      stream.write ("S2", 2);
      wbyte (stream, packetLen);
      wbyte (stream, codeStart >> 16);
      wbyte (stream, codeStart >> 8);
      wbyte (stream, codeStart & 0xFF);

      // packet
      for (uint8_t i = 1; i <= len; i++) {
        wbyte (stream, (mCodeArray[i+pos]) >> 8);
        wbyte (stream, mCodeArray[i+pos] & 0xFF);
        }

      // packet end
      string s = toHex (0xFF - (outputChecksum & 0xFF));
      stream.write (s.c_str(), s.length());
      stream.put ('\n');
      }
    }
  //}}}
  //{{{
  void outputCode (ofstream& stream) {
  // output codeArray

    uint32_t len = mCodeLen;

    mPos = 0;
    while (len > mOutputMaxSize) {
      outputPacket (stream, mPos, mOutputMaxSize);
      mPos = mPos + mOutputMaxSize;
      len = len - mOutputMaxSize;
      }

    if (len > 0)
      outputPacket (stream, mPos, len);
    }
  //}}}

  int mLength = 0;
  eRecordType mType = eNone;

  int mBlockIndex = 0;
  array <uint8_t,256> mBlock = { 0 };

  int outputChecksum = 0;
  uint32_t mOutputMaxSize = 0;

  uint32_t mPos = 0;
  uint32_t mLen = 0;

  uint32_t mCodeStart = 0;
  uint32_t mCodeLen = 0;
  array <uint32_t, 64> mCodeArray;
  };
//}}}

//{{{
void processComment (const string& line) {

  if (kCmdLineDebug)
    printf ("processComment %s\n", line.c_str());
  }
//}}}
//{{{
void processInclude (const string& line, vector <string>& objectFiles) {
// extract include filename and add its contents to the objFilesfile list

  printf ("processInclude %s not implented\n", line.c_str());
  }
//}}}
//{{{
void processObjFile (const string& line, vector <string>& objectFiles) {

  if (kObjectFileDebug)
    printf ("processLine %s\n", line.c_str());

  // find any trailing comma
  size_t foundTerminator = line.find (',');
  if (foundTerminator == string::npos) // no comma, find any trailing cr in linux file i/o
    foundTerminator = line.find ('\r');

  // look for extension dot, assumes only one dot, !!! could search backwards !!!
  size_t foundDot = line.find ('.');
  if (foundDot == string::npos) // no dot, use default ext
    objectFiles.push_back (line.substr (0, foundTerminator) + ".ro");
  else
    objectFiles.push_back (line.substr (0, foundTerminator));
  }
//}}}
//{{{
void processLinker1 (cLinker& linker, const string& fileName) {

  if (kPassDebug)
    printf ("passFile %s\n", fileName.c_str());

  ifstream objFileStream (fileName, ifstream::in);

  bool eom = false;
  cObjectRecord objectRecord;
  while (!eom) {
    eom = objectRecord.loadRoFormat (objFileStream);

    if (kObjectFileDebug)
      objectRecord.dump();

    if (!eom) {
      switch (objectRecord.getType()) {
        case cObjectRecord::eId:
          objectRecord.processId (linker, true);
          break;

        case cObjectRecord::eEsd:
          objectRecord.processEsd (linker, true);
          break;

        case cObjectRecord::eObjectText:
          break;

        case cObjectRecord::eEnd:
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
void processLinker2 (cLinker& linker, const string& fileName, ofstream& stream) {

  if (kPassDebug)
    printf ("passFile %s\n", fileName.c_str());

  ifstream objFileStream (fileName, ifstream::in);

  bool eom = false;
  cObjectRecord objectRecord;
  while (!eom) {
    eom = objectRecord.loadRoFormat (objFileStream);

    if (!eom) {
      switch (objectRecord.getType()) {
        case cObjectRecord::eId:
          objectRecord.processId (linker, false);
          break;

        case cObjectRecord::eEsd:
          objectRecord.processEsd (linker, false);
          break;

        case cObjectRecord::eObjectText:
          objectRecord.processText (linker, stream);
          break;

        case cObjectRecord::eEnd:
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

  // get commandLine args
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
  vector <string> objectFiles;
  ifstream cmdFileStream (cmdFileName + ".cmd", ifstream::in);
  while (getline (cmdFileStream, line))
    if (line[0] == '/')
      linker.processOptions (line);
    else if (line[0] == '!')
      processComment (line);
    else if (line[0] == '#')
      processComment (line);
    else if (line[0] == '@')
      processInclude (line, objectFiles);
    else
      processObjFile (line, objectFiles);
  cmdFileStream.close();

  linker.dumpOptions();

  // pass 1 - read symbols, accumulate section sizes
  for (auto& objectFile : objectFiles)
    processLinker1 (linker, objectFile);
  linker.dumpSymbols();

  // allocate section sizes
  linker.allocCommon();
  linker.dumpSections();

  // pass 2 - resolve addresses and output .bin
  ofstream outFileStream (cmdFileName + ".sr", ofstream::out);
  for (auto& objectFile : objectFiles)
    processLinker2 (linker, objectFile, outFileStream);
  cmdFileStream.close();

  return 0;
  }
//}}}
