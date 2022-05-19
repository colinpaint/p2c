// qlCpp.cpp
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
constexpr bool kSwitchDebug = false;
constexpr bool kCmdLineDebug = false;
constexpr bool kObjFileDebug = false;

constexpr bool kPassDebug = false;
//}}}
//{{{  const, enum
// switches, easier to use as globals
enum eSwitches { eChat, eDebug, eMod, eMap, eBell, eXref, eCheck, eBin, eLastSwitch };
constexpr size_t kNumSwitches = eLastSwitch; // for enum arrays to play nice

// sections
constexpr size_t kNumSections = 16;

// symbol
constexpr size_t kMaxSymbolNameLength = 10;

constexpr uint32_t kStartBase = 0x400;
//}}}

//{{{
class cSwitches {
public:
  cSwitches() {}
  virtual ~cSwitches() = default;

  //{{{
  uint32_t getSectionBaseAddress (size_t section) {
    return mSectionBaseAddress[section];
    }
  //}}}

  //{{{
  void process (const string& line) {

    if (kSwitchDebug)
      printf ("processSwitches %s\n", line.c_str());

    // parse into individual switches, stripping out /
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

    printf ("switches ");
    for (int switchIndex = eChat; switchIndex <= eBin; switchIndex++)
      if (mSwitches[switchIndex])
        printf ("%s ", kSwitchNames [switchIndex].c_str());
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
  void processToken (const string& token) {

    mTokenIndex = 0;
    mToken = token;

    bool found = false;
    for (int switchIndex = eChat; switchIndex <= eBin; switchIndex++)
      if ((token == kSwitchNames[switchIndex]) ||(token == kSwitchAltNames[switchIndex])) {
        mSwitches[switchIndex] = true;
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

        if (kSwitchDebug)
          printf ("processSwitch section %s sectionNum:%2d address:%6x\n", token.c_str(), (int)section, address);
        }
      else
        printf ("processSwitch - unrecognised switch %s\n", token.c_str());
      }
    }
  //}}}

  // const
  const array <string, kNumSwitches> kSwitchNames =    { "chat", "debug", "mod", "map", "bell", "xref", "check", "bin"};
  const array <string, kNumSwitches> kSwitchAltNames = { "cha",  "deb",   "",    "",    "",     "xrf",  "chk",   ""   };

  // var
  array <bool, kNumSwitches> mSwitches = { false };
  array <uint32_t, kNumSections> mSectionBaseAddress = { 0 };

  size_t mTokenIndex = 0;
  string mToken;
  };
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
  int mAddr = 0;
  size_t mComSize = 0;

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
  cSymbol* findInsertSymbol (const string& symbolName, bool& symbolFound) {

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
      mBasePosition = switches.getSectionBaseAddress (symbol->mSection);

      symbol->mAddr = mSectBase[symbol->mSection];
      mSectBase[symbol->mSection] = mSectBase[symbol->mSection] + uint32_t(symbol->mComSize);

      if (mSectBase[symbol->mSection] & 1)
        mSectBase[symbol->mSection] = mSectBase[symbol->mSection] + 1;
      }
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
      uint32_t baseAddress = switches.getSectionBaseAddress (section);
      if (baseAddress)
        mBasePosition = baseAddress;

      if (mSectBase[section]) {
        printf ("section:%2d start:%6x size:%6x\n", (int)section, mBasePosition, mSectBase[section]);
        mBaseAddr[section] = mBasePosition;
        mBasePosition = mBasePosition + mSectBase[section];
        }
      }

    printf ("          finish:%6x size:%6x\n", mBasePosition, mBasePosition - switches.getSectionBaseAddress(0));
    }
  //}}}

  cSwitches switches;

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

    string name;

    // read maxSymbolNameLength, only add up to first space in string
    bool terminated = false;
    for (int i = 0; i < kMaxSymbolNameLength; i++) {
      char byte = getByte();
      if (byte == ' ')
        terminated = true;
      if (!terminated)
        name = name + byte;
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
          string commonSymbolName = getSymbolName();
          uint32_t size = getInt();

          if (kPassDebug)
            printf ("common data - section:%2d %8s size:%08x\n", (int)section, commonSymbolName.c_str(), size);

          if (pass1) {
            //{{{  pass1
            bool symbolFound;
            cSymbol* symbol = linker.findInsertSymbol (commonSymbolName, symbolFound);
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
                  printf ("Label %s is used double defined\n", commonSymbolName.c_str());
                  printf ("as a common in module %s\n", linker.getModName().c_str());
                  printf (" and as an XDEF in module %s\n", symbol->mModName.c_str());
                  symbol->mFlagged = true;
                  }

                else if (!symbol->mFlagged) {  // and check
                  //showModName;
                  printf ("Common area size clash - common %s\n ", commonSymbolName.c_str());
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
          string xdefSymbolName = getSymbolName();
          uint32_t address = getInt();

          if (kPassDebug)
            printf ("symbol xdef - section:%2d %8s address:%08x\n", (int)section, xdefSymbolName.c_str(), address);

          if (pass1) {
            //{{{  pass1
            bool symbolFound;
            cSymbol* symbol = linker.findInsertSymbol (xdefSymbolName, symbolFound);

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
            symbol->mAddr = address + linker.mSectBase[section];
            }
            //}}}
          else {
            //if usingHistory then
            //  begin { symbol defintion, use to make patches on second pass }
            //  b := findInsert (s, symbol, false); { find it }
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
          string xrefSymbolName = getSymbolName();

          if (kPassDebug)
            printf ("symbol xref - section:%2d %8s\n", (int)section, xrefSymbolName.c_str());

          if (pass1) {
            // pass1 action
            bool symbolFound;
            cSymbol* symbol = linker.findInsertSymbol (xrefSymbolName, symbolFound);
            symbol->addReference (linker.getModName());
            }

          else {
            // pass2 action
            cSymbol* symbol = linker.findSymbol (xrefSymbolName);
            if (!symbol) {
              //showModName;
              printf ("internal check failure - lost symbol\n");
              }

            linker.mEsdArray[linker.mTopEsd] = symbol->mAddr + linker.mBaseAddr[symbol->mSection];
            linker.mEsdSymbolArray [linker.mTopEsd] = symbol;

            linker.mOutAddrArray[linker.mTopEsd] = linker.mEsdArray[linker.mTopEsd];
            linker.mTopEsd = linker.mTopEsd + 1;
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

          if (pass1) {
            }
          else {
            }

          break;
          }
        //}}}
        //{{{
        case  9: { // commandLineAddress in absolute section
          if (kPassDebug)
            printf ("command Line address absolute section\n");

          for (int i = 0; i < 5; i++)
            getByte();

          if (pass1) {
            }
          else {
            }

          break;
          }
        //}}}
        //{{{
        case 10: { // commandLineAddress in common section in section xx
          if (kPassDebug)
            printf ("command Line address common section\n");

          for (int i = 0; i < 15; i++)
            getByte();

          if (pass1) {
            }
          else {
            }

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
void processInclude (const string& line) {
// should extract includee filename and add its contents to the objFilesfile list

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


  //baseaddr[-1] := 0;      {set up base of absolute section}


//{{{
int main (int numArgs, char* args[]) {

  cLinker linker;

  // get command line args
  string cmdFileName;
  for (int i = 1; i < numArgs; i++)
    if (args[i][0] == '/')
      linker.switches.process (args[i]);
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
      linker.switches.process (line);
    else if (line[0] == '!')
      processComment (line);
    else if (line[0] == '#')
      processComment (line);
    else if (line[0] == '@')
      processInclude (line);
    else
      processObjFile (line, objFiles);
  cmdFileStream.close();

  linker.switches.dump();

  // read symbols and accumulate section sizes
  for (auto& objFile : objFiles)
    processLinker (linker, objFile.c_str(), true);

  linker.dumpSymbols();
  linker.allocCommon();
  linker.dumpSections();

  // resolve addresses and output .bin
  for (auto& objFile : objFiles)
    processLinker (linker, objFile.c_str(), false);

  return 0;
  }
//}}}
