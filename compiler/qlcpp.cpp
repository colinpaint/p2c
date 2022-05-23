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
constexpr uint32_t kDefaultStartAddress = 0x400;

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
  void addReference (const string& moduleName) {
    mReferences.push_back (moduleName);
    }
  //}}}

  bool mDefined = false;

  string mName;
  string mModuleName;

  uint8_t mSection = 0;
  uint32_t mAddress = 0;

  bool mCommonSizeDefined = false;
  uint32_t mCommonSize = 0;

  bool mUsed = false;
  bool mHist = false;
  bool mErrorFlagged = false;

  vector <string> mReferences;
  };
//}}}
//{{{
class cLinker {
public:
  cLinker() = default;
  virtual ~cLinker() = default;

  //{{{
  string getCurModuleName() {
    return mCurModuleName;
    }
  //}}}
  //{{{
  void setCurModuleName (const string& moduleName) {
    mCurModuleName = moduleName;
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
  cSymbol* findCreateSymbol (const string& symbolName, bool& found) {

    cSymbol* symbol = findSymbol (symbolName);

    found = symbol != nullptr;
    if (!found) {
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
  void allocCommonSectionAddresses() {
  // allocate common section addresses

    for (auto& symbol : mCommonSymbols) {
      mBaseAddress = mOptions.getSectionBaseAddress (symbol->mSection);

      symbol->mAddress = mSections[symbol->mSection].mSectBase;
      mSections[symbol->mSection].mSectBase = mSections[symbol->mSection].mSectBase + uint32_t(symbol->mCommonSize);

      if (mSections[symbol->mSection].mSectBase & 1)
        mSections[symbol->mSection].mSectBase = mSections[symbol->mSection].mSectBase + 1;
      }
    }
  //}}}

  //{{{
  void parseOptions (const string& line) {
    mOptions.parseLine (line);
    }
  //}}}

  //{{{
  void incCommonDefs() {
    mNumCommonDefs++;
    }
  //}}}
  //{{{
  void incXdefs() {
    mNumXdefs++;
    }
  //}}}
  //{{{
  void incXrefs() {
    mNumXrefs++;
    }
  //}}}

  //{{{
  void dumpOptions() {
    mOptions.dump();
    }
  //}}}
  //{{{
  void dumpSections() {

    for (uint8_t section = 0; section < 16; section++) {
      if (mOptions.getSectionBaseAddress (section))
        mBaseAddress = mOptions.getSectionBaseAddress (section);

      if (mSections[section].mSectBase) {
        printf ("section:%2d start:%6x size:%6x\n", (int)section, mBaseAddress, mSections[section].mSectBase);
        mSections[section].mBaseAddress = mBaseAddress;
        mBaseAddress = mBaseAddress + mSections[section].mSectBase;
        }
      }

    printf ("          finish:%6x size:%6x\n", mBaseAddress, mBaseAddress - mOptions.getSectionBaseAddress(0));
    }
  //}}}
  //{{{
  void dumpSymbols() {

    int numUndefined = 0;

    for (auto const& [key, symbol] : mSymbolMap)
      if (symbol->mDefined)
        numUndefined++;

    printf ("%d undefined symbols of total %d, commonDefs:%d, xDefs:%d, xRefs:%d\n",
            numUndefined, (int)mSymbolMap.size(), mNumCommonDefs, mNumXdefs, mNumXrefs);
    }
  //}}}

  uint32_t mBaseAddress = kDefaultStartAddress;

  //{{{
  struct sSection {
    uint32_t mSbase;
    uint32_t mSectBase;
    uint32_t mBaseAddress;
    };
  //}}}
  array <sSection, 16> mSections = {};

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
    void parseLine (const string& line) {

      if (kOptionDebug)
        printf ("processOptions %s\n", line.c_str());

      // parse into individual options, stripping out /
      size_t start = 1;
      size_t found = line.find ('/', start);
      while (found != string::npos)  {
        parseToken (line.substr (start, found-start));
        start = found + 1;
        found = line.find ('/', start);
        }

      parseToken (line.substr (start, found));
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
    void parseToken (const string& token) {

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

  string mCurModuleName;

  uint32_t mNumCommonDefs = 0;
  uint32_t mNumXdefs = 0;
  uint32_t mNumXrefs = 0;
  };
//}}}
//{{{
class cOutput {
public:
  //{{{
  cOutput (const string& fileName) : mOutputMaxSize (bin ? 512 : 16) {
    mStream.open (fileName + ".sr", bin ? ofstream::out | ofstream::binary : ofstream::out);
    }
  //}}}
  //{{{
  virtual ~cOutput() {

    if (bin) {
      writeByte (esc);
      writeByte (0);

      mOutputChecksum = 0;
      writeCheckSummedByte (2);
      writeCheckSummedByte (0);
      writeCheckSummedByte (4);

      for (int i = 0; i < 4; i++)
        writeCheckSummedByte (0);

      // !!! this looks odd, flushes the stream for download ?
      for (int i = 0; i < 512; i++)
        writeByte (0);
      }

    else
      mStream.write ("S9030000FC\n", 11);

    mStream.close();
    }
  //}}}

  //{{{
  void init() {
    mCodeLength = 0;
    }
  //}}}

  //{{{
  void addCodeByte (uint8_t byte) {
    mCodeArray[mCodeLength++] = byte;
    }
  //}}}
  //{{{
  uint32_t outputCode (uint32_t start) {
  // output codeArray

    uint32_t pos = 0;

    uint32_t length = mCodeLength;
    while (length > mOutputMaxSize) {
      outputPacket (start, pos, mOutputMaxSize);
      pos = pos + mOutputMaxSize;
      length = length - mOutputMaxSize;
      }

    if (length > 0)
      outputPacket (start, pos, length);

    // retunr codeLength as number of bytes
    return mCodeLength * 2;
    }
  //}}}

private:
  //{{{
  static string toHex (uint8_t byte) {

    string result;

    uint8_t digit = byte >> 4;
    result += digit > 9 ? 'a' + digit - 10 : '0' + digit;

    digit = byte & 0x0F;
    result += digit > 9 ? 'a' + digit - 10 : '0' + digit;

    return  result;
    }
  //}}}

  //{{{
  void writeByte (uint8_t b) {

    if ((b == esc) && escape)
      mStream.put (b);

    mStream.put (b);
    }
  //}}}
  //{{{
  void writeCheckSummedByte (uint8_t b) {

    if (bin) {
      writeByte (b);
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
  void outputPacket (uint32_t start, uint32_t pos, uint32_t length) {
  // output packet of codeArray

    uint32_t packetAddress = start + (pos * 2);
    uint32_t packetLength = (length * 2) + 4; // this happens to be right for both

    if (bin) {
      //{{{  binary
      writeByte (esc);
      writeByte (0);

      writeCheckSummedByte (1);
      writeCheckSummedByte (packetLength >> 8);
      writeCheckSummedByte (packetLength & 0xFF);

      writeCheckSummedByte (packetAddress >> 24);
      writeCheckSummedByte (packetAddress >> 16);
      writeCheckSummedByte (packetAddress >> 8);
      writeCheckSummedByte (packetAddress & 0xFF);

      // !!!! is this right
      mOutputChecksum = 0;

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

      writeByte (mOutputChecksum);
      }
      //}}}
    else {
      // packet start
      mStream.write ("S2", 2);

      mOutputChecksum = 0;
      writeCheckSummedByte (packetLength);
      writeCheckSummedByte (packetAddress >> 16);
      writeCheckSummedByte (packetAddress >> 8);
      writeCheckSummedByte (packetAddress & 0xFF);

      // packet
      for (uint8_t i = 0; i < length; i++) {
        writeCheckSummedByte ((mCodeArray[i+pos]) >> 8);
        writeCheckSummedByte (mCodeArray[i+pos] & 0xFF);
        }

      // packet end
      string s = toHex (0xFF - (mOutputChecksum & 0xFF));
      mStream.write (s.c_str(), s.length());

      mStream.put ('\n');
      }
    }
  //}}}

  ofstream mStream;

  const uint32_t mOutputMaxSize = 0;

  uint32_t mCodeLength = 0; // code length in words
  int mOutputChecksum = 0;
  array <uint32_t, 64> mCodeArray;
  };
//}}}
//{{{
class cObjectFile {
public:
  cObjectFile (const string& fileName) : mFileName(fileName), mFileType(eRo) {}
  virtual ~cObjectFile() = default;

  //{{{
  void pass1 (cLinker& linker) {

    if (kPassDebug)
      printf ("passFile %s\n", mFileName.c_str());

    ifstream stream (mFileName.c_str(), ifstream::in | ifstream::binary);
    if (!stream.is_open()) {
      //{{{  error, return
      mErrorFlagged = true;
      printf ("error - objectFile %s not found\n", mFileName.c_str());
      return;
      }
      //}}}

    cRecord record;

    bool eom = false;
    while (!eom) {
      eom = record.load (stream);

      if (kObjectFileDebug)
        record.dump();

      if (!eom) {
        switch (record.getType()) {
          case cRecord::eId:
            incNumIdRecords();
            record.parseId (this, linker, true);
            break;

          case cRecord::eEsd:
            incNumEsdRecords();
            record.parseEsd (this, linker, true);
            break;

          case cRecord::eText:
            incNumTxtRecords();
            break;

          case cRecord::eEnd:
            incNumEndRecords();
            break;

          default:
            break;
          }
        }
      }

    stream.close();
    }
  //}}}
  //{{{
  void pass2 (cLinker& linker, cOutput& output) {

    if (mErrorFlagged)
      return;

    if (kPassDebug)
      printf ("passFile %s\n", mFileName.c_str());

    ifstream stream (mFileName.c_str(), ifstream::in | ifstream::binary);
    if (!stream.is_open()) {
      //{{{  error, return
      mErrorFlagged = true;
      printf ("error - objectFile %s not found on pass2 but ok on pass1\n", mFileName.c_str());
      return;
      }
      //}}}

    cRecord record;

    bool eom = false;
    while (!eom) {
      eom = record.load (stream);

      if (!eom) {
        switch (record.getType()) {
          case cRecord::eId:
            record.parseId (this, linker, false);
            break;

          case cRecord::eEsd:
            record.parseEsd (this, linker, false);
            break;

          case cRecord::eText:
            record.parseText (this, linker, output);
            break;

          case cRecord::eEnd:
            break;

          default:
            break;
          }
        }
      }

    stream.close();
    }
  //}}}

  //{{{
  void dump() {

    printf ("objectFile %s id:%d esd:%d text:%d end%d topEsd:%d\n",
            mFileName.c_str(), mNumIdRecords, mNumEsdRecords, mNumTxtRecords, mNumEndRecords, mTopEsd);
    };
  //}}}

  // esd
  //{{{
  struct sEsd {
    cSymbol* mSymbol;
    uint32_t mAddress;
    uint32_t mOutAddress;
    };
  //}}}
  array <sEsd, 256> mEsds = {};
  uint8_t mTopEsd = 0;

private:
  //{{{
  class cRecord {
  public:
    cRecord() = default;
    virtual ~cRecord() = default;

    enum eType { eNone, eId, eEsd, eText, eEnd };

    // load
    //{{{
    bool load (ifstream& stream) {
    // load .ro format record
    // - return true if EOM, should use EOF as well

      mBlockIndex = 0;

      mLength = stream.get();
      uint8_t header = stream.get();

      if ((header > '0') && (header <= '4')) {
        // recognised header type ascii 1,2,3,4
        mType = (eType)(header - uint8_t('0'));
        if (mType != eEnd)
          for (size_t i = 0; i < mLength-1; i++)
            mBlock[i] = stream.get();

        return (mType == eEnd);
        }

      else {
        // unrecognised header type
        mType = eNone;
        return true;
        }
      }
    //}}}

    // gets
    //{{{
    eType getType() {
      return mType;
      };
    //}}}
    //{{{
    int getBytesLeft() {
      return mLength - mBlockIndex - 1;
      }
    //}}}
    //{{{
    uint8_t getUint8() {
    // get 1 bytes to form uint8_t result

      if (mBlockIndex >= mLength) {
        printf ("error - cObject::getUint8 past end of record data\n");
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
      bool spaceFound = false;
      for (int i = 0; i < kObjectSymbolNameLength; i++) {
        char byte = getUint8();
        if (byte == ' ')
          spaceFound = true;
        if (!spaceFound)
          name = name + char(toupper (byte));
        }

      return name;
      }
    //}}}

    // parse
    //{{{
    void parseId (cObjectFile* objectFile, cLinker& linker, bool pass1) {

      string moduleName = getSymbolName();
      linker.setCurModuleName (moduleName);

      if (kPassDebug)
        printf ("Id record - module:%s\n", moduleName.c_str());

      // init esd values in case of zero length sections
      objectFile->mTopEsd = 17;

      if (!pass1) {
        // set esd 0 info, not sure why???
        objectFile->mEsds[0].mAddress = 0;

        // set common esd 1-16 from sections 0-15 info
        for (int section = 0; section < 16; section++) {
          objectFile->mEsds[objectFile->mTopEsd].mSymbol = nullptr;
          objectFile->mEsds[section+1].mAddress = linker.mSections[section].mBaseAddress +
                                                  linker.mSections[section].mSbase;
          objectFile->mEsds[section+1].mOutAddress = objectFile->mEsds[section+1].mAddress;
          }
        }
      }
    //}}}
    //{{{
    void parseEsd (cObjectFile* objectFile, cLinker& linker, bool pass1) {
    // parse ExternalSymbolDefinition record

      while (getBytesLeft() > 0) {
        uint8_t byte = getUint8();
        uint8_t section = byte & 0x0F;
        uint8_t esdType = byte >> 4;

        switch (esdType) {
          //{{{
          case  0: { // absolute section
            uint32_t size = getUint32();
            uint32_t start = getUint32();

            if (pass1)
              printf ("Absolute section module:%s size:%x start:%x\n",
                      linker.getCurModuleName().c_str(), size, start);
            else {
              objectFile->mEsds[objectFile->mTopEsd].mSymbol = nullptr;
              objectFile->mEsds[objectFile->mTopEsd].mAddress = start;
              objectFile->mEsds[objectFile->mTopEsd].mOutAddress = objectFile->mEsds[objectFile->mTopEsd].mAddress;
              objectFile->mTopEsd++;
              }

            break;
            }
          //}}}
          //{{{
          case  1: { // common section xx
            string symbolName = getSymbolName();
            uint32_t size = getUint32();

            if (pass1) {
              linker.incCommonDefs();
              if (kPassDebug)
                printf ("commonSection:%2d %10s size:%x\n", (int)section, symbolName.c_str(), size);

              bool found;
              cSymbol* symbol = linker.findCreateSymbol (symbolName, found);
              symbol->addReference (linker.getCurModuleName());

              if (symbol->mDefined) {
                // check redefinition
                if (size != symbol->mCommonSize) {
                  if (!symbol->mErrorFlagged && !symbol->mCommonSizeDefined) {
                    printf ("Label %s doubleDefined\n", symbolName.c_str());
                    printf ("- common in %s\n", linker.getCurModuleName().c_str());
                    printf ("- xDef in %s\n", symbol->mModuleName.c_str());
                    symbol->mErrorFlagged = true;
                    }
                  else if (!symbol->mErrorFlagged) {
                    // check
                    printf ("Common area size clash - common %s\n ", symbolName.c_str());
                    printf ("- in %s size:%d\n", linker.getCurModuleName().c_str(), int(size));
                    printf ("- in %s size:%d\n", symbol->mModuleName.c_str(), int(symbol->mCommonSize));
                    symbol->mErrorFlagged = true;
                    }

                  if (symbol->mCommonSizeDefined && (size > symbol->mCommonSize)) {
                    symbol->mModuleName = linker.getCurModuleName();
                    symbol->mCommonSize = size;
                    symbol->mCommonSizeDefined = true;
                    }
                  }
                }
              else {
                symbol->mDefined = true;
                symbol->mModuleName = linker.getCurModuleName();
                symbol->mSection = section;
                symbol->mCommonSize = size;
                linker.addCommonSymbol (symbol);
                }
              }
            else {
              // pass2
              cSymbol* symbol = linker.findSymbol (symbolName);
              if (!symbol)
                printf ("error - common symbol not found on pass2 %s\n", symbolName.c_str());

              objectFile->mEsds[objectFile->mTopEsd].mAddress = symbol->mAddress + linker.mSections[symbol->mSection].mBaseAddress;
              objectFile->mEsds[objectFile->mTopEsd].mSymbol = symbol;
              objectFile->mEsds[objectFile->mTopEsd].mOutAddress = objectFile->mEsds[objectFile->mTopEsd].mAddress;
              objectFile->mTopEsd++;
              }

            break;
            }
          //}}}

          case 2:         // standard relocatable section xx
          //{{{
          case  3: { // short address relocatable section xx
            uint32_t size = getUint32();

            if (kPassDebug)
              printf ("relocatable section:%2d size:%x\n", (int)section, size);

            if (pass1) {
              linker.mSections[section].mSectBase += size;
              if (linker.mSections[section].mSectBase & 1)
                linker.mSections[section].mSectBase++;
              }
            else {
              // pass2 - set esd info
              objectFile->mEsds [objectFile->mTopEsd].mSymbol = nullptr;
              objectFile->mEsds[section+1].mAddress = linker.mSections[section].mBaseAddress +
                                                      linker.mSections[section].mSbase;
              objectFile->mEsds[section+1].mOutAddress = objectFile->mEsds[section+1].mAddress;

              // not sure why this is done again ???
              linker.mSections[section].mSbase += size;
              if (linker.mSections[section].mSbase & 1)
                linker.mSections[section].mSbase++;
              }

            break;
            }
          //}}}

          case 4:         // xDef symbol in relocatble section xx
          //{{{
          case  5: { // xDef symbol in absolute section
            string symbolName = getSymbolName();
            uint32_t address = getUint32();

            if (pass1) {
              linker.incXdefs();

              bool found;
              cSymbol* symbol = linker.findCreateSymbol (symbolName, found);
              if (kPassDebug)
                printf ("symbol xDef %s - section:%2d module:%s %s symbol:%s address:%x\n",
                        found ? "redefined": "",
                        (int)section,
                        linker.getCurModuleName().c_str(),
                        found ? symbol->mModuleName.c_str() : "",
                        symbolName.c_str(), address);

              symbol->mDefined = true;
              symbol->mModuleName = linker.getCurModuleName();
              symbol->mSection = section;
              symbol->mAddress = address + linker.mSections[section].mSectBase;
              }

            break;
            }
          //}}}

          case 6:         // xRef symbol to section xx - unexpected
            printf ("xRef to specified section:%d\n", int(section));
            [[fallthrough]];
          //{{{
          case  7: { // xRef symbol to any section
            string symbolName = getSymbolName();

            if (kPassDebug)
              printf ("symbol xRef - section:%2d %8s\n", (int)section, symbolName.c_str());

            if (pass1) {
              linker.incXrefs();

              // find, or create symbol undefined
              bool found;
              cSymbol* symbol = linker.findCreateSymbol (symbolName, found);

              // add reference to symbol's references
              symbol->addReference (linker.getCurModuleName());
              }

            else {
              // pass2
              cSymbol* symbol = linker.findSymbol (symbolName);
              if (symbol) {
                // set esd info
                objectFile->mEsds[objectFile->mTopEsd].mSymbol = symbol;
                objectFile->mEsds[objectFile->mTopEsd].mAddress = symbol->mAddress + linker.mSections[symbol->mSection].mBaseAddress;
                objectFile->mEsds[objectFile->mTopEsd].mOutAddress = objectFile->mEsds[objectFile->mTopEsd].mAddress;
                objectFile->mTopEsd++;
                }
              else
                printf ("error - xRef symbol not found in pass2 %s\n", symbolName.c_str());
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
    void parseText (cObjectFile* objectFile, cLinker& linker, cOutput& output) {
    // parse text record, to out stream, pass 2 only

      if (kOutDebug)
        dump();

      uint32_t bitmap = getUint32();
      uint8_t curEsd = getUint8();
      uint32_t codeStart = objectFile->mEsds[curEsd].mOutAddress;

      if (kOutDebug)
        printf ("output bitmap:%x curEsd:%d\n", bitmap, curEsd);

      output.init();

      uint8_t thisEsd = 0;
      while (getBytesLeft() > 0) {
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
              printf ("thisEsd:%d topEsd:%d\n", thisEsd, objectFile->mTopEsd);
            if (thisEsd > objectFile->mTopEsd)
              //{{{  error, using esd greater than topEsd
              printf ("error - %s using esd:%d greater than topEsd:%d\n",
                      linker.getCurModuleName().c_str(), thisEsd, objectFile->mTopEsd);
              //}}}

            if (i & 0x1)
              add = add + objectFile->mEsds[thisEsd].mAddress;
            else
              add = add - objectFile->mEsds[thisEsd].mAddress;
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
            printf ("offset %x + %x = %x\n", add, offset, add + offset);

          add = add + offset;
          if (numEsds == 0) {
            //{{{  not sure what this does
            if (offset & 0x01) {
              printf ("error - %s odd fix-up offset:%8x esd:%d, codeStart:%8x\n",
                      linker.getCurModuleName().c_str(), offset, curEsd, codeStart);
              offset = offset + 1;
              }

            uint32_t codeBytes = output.outputCode (codeStart);
            objectFile->mEsds[curEsd].mOutAddress = objectFile->mEsds[curEsd].mOutAddress + codeBytes + offset;

            output.init();
            codeStart = objectFile->mEsds[curEsd].mOutAddress;
            }
            //}}}
          else {
            if (!longAddress && (add & 0xFFFF0000))
              printf ("error - trying to put long address into word location:%8x\n", add);

            if (objectFile->mEsds [thisEsd].mSymbol != nullptr) {
              // only need named symbols
              if (linker.getCurModuleName() != objectFile->mEsds[thisEsd].mSymbol->mModuleName) {
                //{{{  outside module
                //if history then
                //  { address to be resolved longAddress only at present}
                //  addRes (esdSymbolArray [thisesd], codeStart + mCodeLen*2, offset);

                //if debugInfo then
                //  writeln ('sym ', longAddress,
                //           ' ', thisesd:2,
                //           ' ', esdSymbolArray [thisesd]^.symbolName,
                //           ' ', hex (add, 8, 8), ' = ', hex (esdArray[thisesd]),
                //           ' + ', hex (offset, 4, 4), ';', hex (offsetFieldLength, 1, 1),
                //           ' at ', hex (codeStart + mCodeLen * 2, 8, 8));
                }
                //}}}

              // generate resolved address
              if (longAddress)
                output.addCodeByte (add >> 16);
              output.addCodeByte (add);
              }
            }
          }
          //}}}
        else {
          //{{{  absolute code
          output.addCodeByte (getUint8());
          output.addCodeByte (getUint8());
          }
          //}}}
        bitmap = bitmap << 1;
        }

      objectFile->mEsds[curEsd].mOutAddress += output.outputCode (codeStart);
      }
    //}}}

    // dump
    //{{{
    void dump() {

      if (mType == eEnd)
        printf ("end\n");

      else {
        printf ("record length:0x%x type:%d\n", mLength, (int)mType);

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
    eType mType = eNone;
    int mBlockIndex = 0;
    array <uint8_t,256> mBlock = { 0 };
    };
  //}}}

  //{{{
  void incNumIdRecords() {
    mNumIdRecords++;
    }
  //}}}
  //{{{
  void incNumEsdRecords() {
    mNumEsdRecords++;
    }
  //}}}
  //{{{
  void incNumTxtRecords() {
    mNumTxtRecords++;
    }
  //}}}
  //{{{
  void incNumEndRecords() {
    mNumEndRecords++;
    }
  //}}}

  string mFileName;
  bool mErrorFlagged = false;

  enum eFileType { eRo, eRx };
  eFileType mFileType;

  uint32_t mNumIdRecords = 0;
  uint32_t mNumEsdRecords = 0;
  uint32_t mNumTxtRecords = 0;
  uint32_t mNumEndRecords = 0;
  };
//}}}

//{{{
void parseStream (ifstream& stream, vector <cObjectFile>& objectFiles, cLinker& linker) {

  string line;
  while (getline (stream, line))
    if (line[0] == '/')
      linker.parseOptions (line);
    else if (line[0] == '@') {
      //{{{  include
      string includeFileName = line.substr (1, line.length()-1) + ".cmd";
      printf ("including file %s\n", includeFileName.c_str());

      ifstream includeStream (includeFileName, ifstream::in);
      if (!includeStream.is_open())
        printf ("error - include file %s not found\n", includeFileName.c_str());
      else {
        parseStream (includeStream, objectFiles, linker);
        includeStream.close();
        }
      }
      //}}}
    else if (line[0] == '!') {
      //{{{  comment
      if (kCmdLineDebug)
        printf ("comment %s\n", line.c_str());
      }
      //}}}
    else if (line[0] == '#') {
      //{{{  comment
      if (kCmdLineDebug)
        printf ("comment %s\n", line.c_str());
      }
      //}}}
    else {
      //{{{  objectFileName
      if (kObjectFileDebug)
        printf ("objectFileName %s\n", line.c_str());

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
  }
//}}}

//{{{
int main (int numArgs, char* args[]) {

  cLinker linker;

  // get commandLine args
  string cmdFileName;
  for (int i = 1; i < numArgs; i++)
    if (args[i][0] == '/')
      linker.parseOptions (args[i]);
    else
      cmdFileName = args[i];

  if (cmdFileName.empty()) {
    //{{{  no .cmd filename - error, exit
    printf ("no .cmd file specified\n");
    return 1;
    }
    //}}}

  printf ("using cmdFileName %s\n", cmdFileName.c_str());

  vector <cObjectFile> objectFiles;

  // read option,objectFiles from .cmd file
  ifstream stream (cmdFileName + ".cmd", ifstream::in);
  if (!stream.is_open()) {
    //{{{  error, return
    printf ("error - cmd file %s not found\n", cmdFileName.c_str());
    return 1;
    }
    //}}}
  parseStream (stream, objectFiles, linker);
  stream.close();

  // show options
  linker.dumpOptions();

  // pass 1 - read symbols, accumulate section sizes
  for (auto& objectFile : objectFiles)
    objectFile.pass1 (linker);
  linker.dumpSymbols();

  // allocate sections
  linker.allocCommonSectionAddresses();
  linker.dumpSections();

  // pass 2 - resolve addresses, output .sr or .bin
  if (out) {
    cOutput output (cmdFileName);
    for (auto& objectFile : objectFiles)
      objectFile.pass2 (linker, output);
    }

  return 0;
  }
//}}}
