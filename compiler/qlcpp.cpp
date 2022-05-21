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
constexpr bool kOutDebug = false;
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
  bool mErrorFlagged = false;
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
  uint8_t getTopEsd() {
    return mTopEsd;
    }
  //}}}
  //{{{
  void incTopEsd() {
    mTopEsd++;
    }
  //}}}
  //{{{
  void initTopEsd() {
    mTopEsd = 17;
    mEsdArray[0] = 0;  // unused Esd value
    }
  //}}}

  //{{{
  string getCurModName() {
    return mCurModName;
    }
  //}}}
  //{{{
  void setCurModName (const string& modName) {
    mCurModName = modName;
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
  array <uint32_t, 16> mSbase = { 0 };
  array <uint32_t, 16> mSectBase = { 0 };
  array <uint32_t, 16> mBaseAddr = { 0 };

  array <uint32_t, 256> mEsdArray = { 0 };
  array <uint32_t, 256> mOutAddrArray = { 0 };
  array <cSymbol*, 256> mEsdSymbolArray = { nullptr };

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

  int mTopEsd = 0;
  string mCurModName;
  };
//}}}
//{{{
class cCode {
public:
  cCode (ofstream& stream) : mStream(stream), mOutputMaxSize (bin ? 512 : 64), mCodeLength(0) {}
  ~cCode() = default;

  //{{{
  void initCode() {
    mCodeLength = 0;
    }
  //}}}
  //{{{
  void initLength() {
    mCodeLength = 0;
    }
  //}}}

  //{{{
  uint32_t getLength() {
    return mCodeLength;
    }
  //}}}

  //{{{
  void addByte (uint8_t byte) {
    mCodeArray[++mCodeLength] = byte;
    }
  //}}}
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
  void sendBin (uint8_t b) {

    if ((b == esc) && escape)
      mStream.put (b);

    mStream.put (b);
    }
  //}}}
  //{{{
  void wbyte (uint8_t b) {

    if (bin) {
      sendBin (b);
      mOutputChecksum ^= b;
      }
    else {
      string s = toHex (b);
      mStream.write (s.c_str(), s.length());
      mOutputChecksum += b;
      }
    }
  //}}}
  //{{{
  void outputPacket (uint32_t start, uint32_t pos, uint32_t len) {
  // output packet of codeArray

    uint32_t codeStart = start + pos*2;
    uint32_t packetLen = len*2 + 4; // this happens to be right for both

    if (bin) {
      //{{{  binary
      sendBin (esc);
      sendBin (0);
      wbyte (1);
      wbyte (packetLen >> 8);
      wbyte (packetLen & 0xFF);
      wbyte (codeStart >> 24);
      wbyte (codeStart >> 16);
      wbyte (codeStart >> 8);
      wbyte (codeStart & 0xFF);

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

      sendBin (mOutputChecksum);
      }
      //}}}
    else {
      // packet start
      mStream.write ("S2", 2);
      wbyte (packetLen);
      wbyte (codeStart >> 16);
      wbyte (codeStart >> 8);
      wbyte (codeStart & 0xFF);

      // packet
      for (uint8_t i = 1; i <= len; i++) {
        wbyte ((mCodeArray[i+pos]) >> 8);
        wbyte (mCodeArray[i+pos] & 0xFF);
        }

      // packet end
      string s = toHex (0xFF - (mOutputChecksum & 0xFF));
      mStream.write (s.c_str(), s.length());
      mStream.put ('\n');
      }
    }
  //}}}
  //{{{
  void outputCode (uint32_t start) {
  // output codeArray

    mPos = 0;
    uint32_t length = mCodeLength;
    while (length > mOutputMaxSize) {
      outputPacket (start, mPos, mOutputMaxSize);
      mPos = mPos + mOutputMaxSize;
      length = length - mOutputMaxSize;
      }

    if (length > 0)
      outputPacket (start, mPos, length);
    }
  //}}}
  //{{{
  void outputEnd() {

    if (bin) {
      mOutputChecksum = 0;
      sendBin (esc);
      sendBin (0);
      wbyte (2);
      wbyte (0);
      wbyte (4);

      for (int i = 1; i <= 4; i++)
        wbyte (0);
      //endPacket();

      for (int i = 0; i <= 512; i++)
        sendBin (0);
      }

    else {
      mStream.write ("S9030000FC", 10);
      mStream.put ('\n');
      }
    }
  //}}}

private:
  ofstream& mStream;

  uint32_t mOutputMaxSize = 0;

  uint32_t mPos = 0;

  uint32_t mCodeLength = 0;
  int mOutputChecksum = 0;
  array <uint32_t, 64> mCodeArray;
  };
//}}}
//{{{
class cObjectRecord {
public:
  cObjectRecord() {}
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
      printf ("error - cObjectRecord::getUint8 past end of record data\n");
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
          printf ("error - symbolName trailing space problem pos:%d value:%02x\n", i, byte);
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

    linker.initTopEsd();

    string modName = getSymbolName();
    linker.setCurModName (modName);

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
        linker.mEsdSymbolArray[linker.getTopEsd()] = nullptr;
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
            linker.mEsdArray[linker.getTopEsd()] = start;
            linker.mEsdSymbolArray [linker.getTopEsd()] = nullptr;

            linker.mOutAddrArray[linker.getTopEsd()] = linker.mEsdArray[linker.getTopEsd()];
            linker.incTopEsd();
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
            symbol->addReference (linker.getCurModName());

            if (!symbol->mDefined) {
              symbol->mModName = linker.getCurModName();
              symbol->mSection = section;
              symbol->mComSize = size;
              symbol->mDefined = true;
              linker.addCommonSymbol (symbol);
              }

            else {
              if (size != symbol->mComSize) {
                if (!symbol->mErrorFlagged && !symbol->mComSizeDefined) {
                  printf ("Label %s doubleDefined\n", symbolName.c_str());
                  printf ("- common in %s\n", linker.getCurModName().c_str());
                  printf ("- xDef in %s\n", symbol->mModName.c_str());
                  symbol->mErrorFlagged = true;
                  }

                else if (!symbol->mErrorFlagged) {
                  // check
                  printf ("Common area size clash - common %s\n ", symbolName.c_str());
                  printf ("- in %s size:%d\n", linker.getCurModName().c_str(), int(size));
                  printf ("- in %s size:%d\n", symbol->mModName.c_str(), int(symbol->mComSize));
                  symbol->mErrorFlagged = true;
                  }

                if (symbol->mComSizeDefined && (size > symbol->mComSize)) {
                  symbol->mModName = linker.getCurModName();
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
            linker.mEsdSymbolArray [linker.getTopEsd()] = nullptr;

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
            if (symbol->mDefined && !symbol->mErrorFlagged) {
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

            symbol->mModName = linker.getCurModName();
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
            symbol->addReference (linker.getCurModName());
            }

          else {
            // pass2 action
            cSymbol* symbol = linker.findSymbol (symbolName);
            if (symbol) {
              linker.mEsdArray[linker.getTopEsd()] = symbol->mAddress + linker.mBaseAddr[symbol->mSection];
              linker.mEsdSymbolArray [linker.getTopEsd()] = symbol;
              linker.mOutAddrArray[linker.getTopEsd()] = linker.mEsdArray[linker.getTopEsd()];
              linker.incTopEsd();
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

    cCode code (stream);

    if (kOutDebug)
      dump();

    uint32_t bitmap = getUint32();
    uint8_t curEsd = getUint8();
    mCodeStart = linker.mOutAddrArray[curEsd];

    if (kOutDebug)
      printf ("output bitmap:%08x curEsd:%d\n", bitmap, curEsd);

    code.initCode();

    uint8_t thisEsd = 0;
    while (getDataLeft() > 0) {
      if (bitmap & 0x80000000) {
        //{{{  relocation data
        uint8_t byte = getUint8();
        uint8_t numEsds = byte >> 5;
        uint8_t offsetFieldLength = byte & 0x07;
        bool longAddress = (byte >> 3) & 1;

        if (kOutDebug)
          printf ("byte:%02x numEsds:%d offsetFieldLength:%d longAddress:%d\n",
                  byte, numEsds, offsetFieldLength, longAddress);

        uint32_t add = 0;
        for (int i = 1; i <= numEsds; i++) {
          thisEsd = getUint8();
          if (kOutDebug)
            printf ("thisEsd:%d topEsd:%d\n", thisEsd, linker.getTopEsd());
          if (thisEsd > linker.getTopEsd())
            //{{{  error, using esd greater than topEsd
            printf ("error - %s using esd:%d greater than topEsd:%d\n",
                    linker.getCurModName().c_str(), thisEsd, linker.getTopEsd());
            //}}}

          if (i & 0x1)
            add = add + linker.mEsdArray[thisEsd];
          else
            add = add - linker.mEsdArray[thisEsd];
          }

        // get offset, either byte or word, sign extend
        uint32_t offset = 0;
        for (int i = 1; i <= offsetFieldLength; i++)
          offset = (offset << 8) + getUint8();

        switch (offsetFieldLength) {
          case 0: break;
          case 1: if (offset & 0x80)
                    offset = 0xFFFFFF00 | offset;
                  break;
          case 2: if (offset & 0x8000)
                    offset = 0xFFFF0000 | offset;
                   break;
          case 4: break;
          default: printf ("error - unexpected offsetFieldLength:%d\n", offsetFieldLength);
          }

        if (kOutDebug)
          printf ("offset %08x + %08x = %08x\n", add, offset, add + offset);

        add = add + offset;
        if (numEsds == 0) {
          //{{{  not sure what this does
          if (offset & 0x01) {
            printf ("error - %s odd fix-up offset:%8x esd:%d, codeStart:%8x\n",
                    linker.getCurModName().c_str(), offset, curEsd, mCodeStart);
            offset = offset + 1;
            }

          code.outputCode (mCodeStart);

          linker.mOutAddrArray[curEsd] = linker.mOutAddrArray[curEsd] + code.getLength()*2 + offset;

          code.initLength();
          mCodeStart = linker.mOutAddrArray[curEsd];
          }
          //}}}
        else {
          if (!longAddress && (add & 0xFFFF0000))
            printf ("error - trying to put long address into word location:%8x\n", add);

          if (linker.mEsdSymbolArray [thisEsd] != nullptr) {
            // only need named symbols
            if (linker.getCurModName() != linker.mEsdSymbolArray [thisEsd]->mModName) {
              //{{{  outside module
              //if history then
              //  { address to be resolved longAddress only at present}
              //  addRes (esdSymbolArray [thisesd], mCodeStart + mCodeLen*2, offset);

              //if debugInfo then
              //  writeln ('sym ', longAddress,
              //           ' ', thisesd:2,
              //           ' ', esdSymbolArray [thisesd]^.symbolName,
              //           ' ', hex (add, 8, 8), ' = ', hex (esdArray[thisesd]),
              //           ' + ', hex (offset, 4, 4), ';', hex (offsetFieldLength, 1, 1),
              //           ' at ', hex (mCodeStart + mCodeLen * 2, 8, 8));
              }
              //}}}

            // generate resolved address
            if (longAddress)
              code.addByte (add >> 16);
            code.addByte (add);
            }
          }
        }
        //}}}
      else {
        //{{{  absolute code
        code.addByte (getUint8());
        code.addByte (getUint8());
        }
        //}}}
      bitmap = bitmap << 1;
      }

    code.outputCode (mCodeStart);

    // convert to bytes}
    linker.mOutAddrArray[curEsd] = linker.mOutAddrArray[curEsd] + (code.getLength() * 2);
    }
  //}}}

  // dump
  //{{{
  void dump() {

    if (mType == eEnd)
      printf ("end\n");

    else {
      printf ("len:0x%x type:%d\n", mLength, (int)mType);

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
  array <uint8_t,256> mBlock = { 0 };

  uint32_t mCodeStart = 0;
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

  int numEsdRecords = 0;
  int numTxtRecords = 0;

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
          numEsdRecords++;
          objectRecord.processEsd (linker, false);
          break;

        case cObjectRecord::eObjectText:
          numTxtRecords++;
          objectRecord.processText (linker, stream);
          break;

        case cObjectRecord::eEnd:
          break;

        default:
          break;
        }
      }
    }

  printf ("%s numEsdRecords:%d numTxtRecords:%d\n", fileName.c_str(), numEsdRecords, numTxtRecords);

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

  if (out) {
    // pass 2 - resolve addresses and output .bin
    ofstream outFileStream (cmdFileName + ".sr", ofstream::out);

    for (auto& objectFile : objectFiles)
      processLinker2 (linker, objectFile, outFileStream);

    cCode code (outFileStream);
    code.outputEnd();

    cmdFileStream.close();
    }

  return 0;
  }
//}}}
