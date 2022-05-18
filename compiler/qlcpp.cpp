// qlCpp.cpp
//{{{  includes
#include <cstdio>
#include <cstdint>

#include <string>

#include <vector>
#include <array>

#include <fstream>

using namespace std;
//}}}

//{{{  const, enum
// const
constexpr bool kSwitchDebug = false;
constexpr bool kCmdLineDebug = false;
constexpr bool kObjFileDebug = false;
constexpr bool kPass1Debug = true;
constexpr bool kPass2Debug = true;

// switches, easier to use as globals
enum eSwitches { eChat, eDebug, eMod, eMap, eBell, eXref, eCheck, eBin, eLastSwitch };
constexpr size_t kNumSwitches = eLastSwitch; // for enum arrays to play nice

// sections
constexpr size_t kNumSections = 16;
//}}}

//{{{
class cObjRecord {
public:
  cObjRecord() {}
  virtual ~cObjRecord() = default;

  enum eRecordType { eNone, eModule, eESD, eText, eEOM };

  //{{{
  bool getRO (ifstream& stream) {

    mLength = stream.get();
    uint8_t b = stream.get();
    mBlockIndex = 0;

    if ((b > '0') && (b <= '4')) {
      mType = (eRecordType)(b - uint8_t('0'));
      if (mType < eEOM)
        for (int i = 0; i < mLength-1; i++)
          mBlock[i] = stream.get();

      return mType == eEOM;
      }

    else {
      mType = eNone;
      return true;
      }
    }
  //}}}

  eRecordType getType() { return mType; }
  //{{{
  string getSymbolName() {

    string name;
    name.resize (10);

    for (int i = 0; i < 10; i++) {
      char ch = getByte();
      if (ch == ' ')
        break;
      name = name + ch;
      }

    return name;
    }
  //}}}

  //{{{
  void report() {

    if (mType == eEOM)
      printf ("EOM\n");

    else {
      printf ("len:%d type:%d\n", (int)mLength, (int)mType);

      // report block
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

  //{{{
  string processModuleId() {
    string modName = getSymbolName();
    return modName;
    }
  //}}}

private:
  //{{{
  uint8_t getByte() {

    if (mBlockIndex >= mLength) {
      printf ("cObjRecord::getByte past end of block\n");
      return 0;
      }

    return mBlock[mBlockIndex++];
    }
  //}}}

  uint8_t mLength = 0;
  eRecordType mType = eNone;

  array <uint8_t,255> mBlock = {0};
  uint8_t mBlockIndex = 0;
  };
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
      processSwitch (line.substr (start, found-start));
      start = found + 1;
      found = line.find ('/', start);
      }

    processSwitch (line.substr (start, found));
    }
  //}}}
  //{{{
  void report() {

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
  int getInt() {

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
  int getHex() {

    int value = 0;
    while (true) {
      char ch = getCh();
      if ((ch >= '0') && (ch <= '9'))
        value = (value * 16) + (ch - '0');
      else if ((ch >= 'A') && (ch <= 'F'))
        value = (value * 16) + ((ch - 'A') + 10);
      else if ((ch >= 'a') && (ch <= 'f'))
        value = (value * 16) + ((ch - 'a') + 10);
      else
        return value;
      }
    }
  //}}}

  //{{{
  void processSwitch (const string& token) {

    mToken = token;
    mTokenIndex = 0;

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
        int section = getInt();
        int address = getHex();
        if ((section >= 0) && (section <= 15))
          mSectionBaseAddress[section] = address;

        if (kSwitchDebug)
          printf ("processSwitch section %s sectionNum:%2d address:%6x\n", token.c_str(), section, address);
        }
      else
        printf ("processSwitch - unrecognised switch %s\n", token.c_str());
      }
    }
  //}}}

  //{{{  const
  const array <string, kNumSwitches> kSwitchNames =    { "chat", "debug", "mod", "map", "bell", "xref", "check", "bin"};
  const array <string, kNumSwitches> kSwitchAltNames = { "cha",  "deb",   "",    "",    "",     "xrf",  "chk",   ""   };
  //}}}

  array <bool, kNumSwitches> mSwitches = { false };
  array <int, kNumSections> mSectionBaseAddress = { 0 };

  string mToken;
  size_t mTokenIndex = 0;
  };
//}}}
//{{{
class cLink {
public:
  cLink() {}
  virtual ~cLink() = default;

private:
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

  // look for trailing comma
  size_t foundComma = line.find (',');

  // look for extension dot, assumes only dot
  size_t foundDot = line.find ('.');
  if (foundDot == string::npos) // no dot, use default ext
    objFiles.push_back (line.substr (0, foundComma) + ".ro");
  else
    objFiles.push_back (line.substr (0, foundComma));
  }
//}}}

//{{{
void pass1File (const string& fileName, cSwitches& switches, const cLink& link) {

  if (kPass1Debug)
    printf ("pass1file %s\n", fileName.c_str());

  ifstream objFileStream (fileName, ifstream::in);

  bool eom = false;
  cObjRecord objRecord;
  while (!eom) {
    eom = objRecord.getRO (objFileStream);
    if (!eom) {
      if (kObjFileDebug)
        objRecord.report();

      switch (objRecord.getType()) {
        case cObjRecord::eModule: {
          string modName = objRecord.processModuleId();
          printf ("ModuleId - modName:%s\n", modName.c_str());
          break;
          }
        case cObjRecord::eESD:
          break;
        case cObjRecord::eText:
          break;
        case cObjRecord::eEOM:
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
void pass2File (const string& fileName, cSwitches& switches, const cLink& link) {

  if (kPass2Debug)
    printf ("pass2file %s\n", fileName.c_str());

  ifstream objFileStream (fileName, ifstream::in);

  bool eom = false;
  cObjRecord objRecord;
  while (!eom) {
    eom = objRecord.getRO (objFileStream);
    if (!eom) {
      if (kObjFileDebug)
        objRecord.report();

      switch (objRecord.getType()) {
        case cObjRecord::eModule:
          break;
        case cObjRecord::eESD:
          break;
        case cObjRecord::eText:
          break;
        case cObjRecord::eEOM:
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

  string cmdFileName;
  cSwitches switches;

  // get cmd line args
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
  vector <string> objFiles;
  string line;

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

  switches.report();

  // process object files
  cLink link;

  for (auto& objFile : objFiles)
    pass1File (objFile.c_str(), switches, link);
  for (auto& objFile : objFiles)
    pass2File (objFile.c_str(), switches, link);

  return 0;
  }
//}}}
