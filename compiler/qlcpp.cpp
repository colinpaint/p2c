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
constexpr bool kPass1Debug = true;
constexpr bool kPass2Debug = true;
//}}}
//{{{  const, enum
// switches, easier to use as globals
enum eSwitches { eChat, eDebug, eMod, eMap, eBell, eXref, eCheck, eBin, eLastSwitch };
constexpr size_t kNumSwitches = eLastSwitch; // for enum arrays to play nice

// sections
constexpr size_t kNumSections = 16;

// symbol
constexpr size_t kMaxSymbolNameLength = 10;
//}}}

//{{{
class cSymbol {
public:
  cSymbol() {}
  virtual ~cSymbol() = default;

private:
  string mName;
  string mModName;
  int section = 0;
  int addr = 0;
  int comsize = 0;
  bool def = false;
  bool used = false;
  bool flagged = false;
  bool hist = false;
  };
//}}}
//{{{
class cLink {
public:
  cLink() {}
  virtual ~cLink() = default;

  void setCurModName (const string& modName) {
    mCurModName = modName;
    }

  void addSymbol (const string& symbol) {
    mNumSymbols++;
    }
  void dump() {
    printf ("symbols:%d %d\n", mNumSymbols, (int)mSymbols.size());
    }

private:
  int mNumSymbols = 0;
  string mCurModName;
  map <string, cSymbol> mSymbols;
  };
//}}}
//{{{
class cSwitches {
public:
  cSwitches() {}
  virtual ~cSwitches() = default;

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
class cObjRecord {
public:
  cObjRecord() {}
  virtual ~cObjRecord() = default;

  enum eRecordType { eNone, eId, eESD, eObjectText, eEnd };

  int getDataLeft() { return mLength - mBlockIndex - 1; }
  eRecordType getType() { return mType; }
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

  //{{{
  void processModuleId (const cSwitches& switches, cLink& link) {
    string modName = getSymbolName();
    link.setCurModName (modName);

    if (kPassDebug)
      printf ("ModuleId - modName:%s\n", modName.c_str());
    }
  //}}}
  //{{{
  void processEsdPass1 (const cSwitches& switches, cLink& link) {
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

          if (kPass1Debug)
            printf ("Absolute %08x %08x\n", size, start);

          break;
          }
        //}}}
        //{{{
        case  1: { // common section xx
          string commonSymbolName = getSymbolName();
          uint32_t size = getInt();

          if (kPass1Debug)
            printf ("common data - section:%2d %8s size:%08x\n", (int)section, commonSymbolName.c_str(), size);

          link.addSymbol (commonSymbolName);
          break;
          }
        //}}}
        case 2:         // standard relocatable section xx
        //{{{
        case  3: { // short address relocatable section xx
          uint32_t size = getInt();
          if (kPass1Debug)
            printf ("section def - section:%2d size:%08x\n", (int)section, size);
          break;
          }
        //}}}
        case 4:         // external symbol defintion in relocatble section xx
        //{{{
        case  5: { // external symbol defintion in absolute section
          string xdefSymbolName = getSymbolName();
          uint32_t address = getInt();

          if (kPass1Debug)
            printf ("symbol xdef - section:%2d %8s address:%08x\n", (int)section, xdefSymbolName.c_str(), address);

          link.addSymbol (xdefSymbolName);

          break;
          }
        //}}}
        case 6:         // external symbol reference to section xx
        //{{{
        case  7: { // external symbol reference to any section
          string xrefSymbolName = getSymbolName();

          if (kPass1Debug)
            printf ("symbol xref - section:%2d %8s\n", (int)section, xrefSymbolName.c_str());
          break;
          }
        //}}}
        //{{{
        case  8: { // command line address in section
          if (kPass1Debug)
            printf ("command Line address section\n");

          for (int i = 0; i < 15; i++)
            getByte();

          break;
          }
        //}}}
        //{{{
        case  9: { // command line address in absolute section
          if (kPass1Debug)
            printf ("command Line address absolute section\n");

          for (int i = 0; i < 5; i++)
            getByte();

          break;
          }
        //}}}
        //{{{
        case 10: { // command line address in common section in section xx
          if (kPass1Debug)
            printf ("command Line address common section\n");

          for (int i = 0; i < 15; i++)
            getByte();

          break;
          }
        //}}}
        //{{{
        default:
          printf ("unknown esdType %d\n", esdType);
        //}}}
        }
      }
    }
  //}}}
  //{{{
  void processEsdPass2 (const cSwitches& switches, cLink& link) {
    }
  //}}}
  //{{{
  void processTextPass2 (const cSwitches& switches, cLink& link) {
    }
  //}}}

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
void pass1File (const string& fileName, cSwitches& switches, cLink& link) {

  if (kPass1Debug)
    printf ("pass1file %s\n", fileName.c_str());

  ifstream objFileStream (fileName, ifstream::in);

  bool eom = false;
  cObjRecord objRecord;
  while (!eom) {
    eom = objRecord.loadRoFormat (objFileStream);
    if (!eom) {
      if (kObjFileDebug)
        objRecord.dump();

      switch (objRecord.getType()) {
        case cObjRecord::eId:
          objRecord.processModuleId (switches, link);
          break;

        case cObjRecord::eESD:
          objRecord.processEsdPass1 (switches, link);
          break;

        case cObjRecord::eObjectText:
          break;

        case cObjRecord::eEnd:
          break;

        default:
          printf ("unrecognised objRecord %d\n", objRecord.getType());
          break;
        }
      }
    }

  objFileStream.close();
  }
//}}}
//{{{
void pass2File (const string& fileName, cSwitches& switches, cLink& link) {

  if (kPass2Debug)
    printf ("pass2file %s\n", fileName.c_str());

  ifstream objFileStream (fileName, ifstream::in);

  bool eom = false;
  cObjRecord objRecord;
  while (!eom) {
    eom = objRecord.loadRoFormat (objFileStream);
    if (!eom) {
      switch (objRecord.getType()) {
        case cObjRecord::eId:
          objRecord.processModuleId (switches, link);
          break;

        case cObjRecord::eESD:
          objRecord.processEsdPass2 (switches, link);
          break;

        case cObjRecord::eObjectText:
          objRecord.processTextPass2 (switches, link);
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

  // get command line args
  string cmdFileName;
  cSwitches switches;
  for (int i = 1; i < numArgs; i++)
    if (args[i][0] == '/')
      switches.process (args[i]);
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
  while (getline (cmdFileStream, line)) {
    if (line[0] == '/')
      switches.process (line);
    else if (line[0] == '!')
      processComment (line);
    else if (line[0] == '#')
      processComment (line);
    else if (line[0] == '@')
      processInclude (line);
    else
      processObjFile (line, objFiles);
    }
  cmdFileStream.close();

  switches.dump();

  // read symbols and accumulate section sizes
  cLink link;
  for (auto& objFile : objFiles)
    pass1File (objFile.c_str(), switches, link);
  link.dump();

  // resolve addresses and output .bin
  for (auto& objFile : objFiles)
    pass2File (objFile.c_str(), switches, link);

  return 0;
  }
//}}}
