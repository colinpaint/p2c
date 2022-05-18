// qlCpp.cpp
//{{{  includes
#include <cstdio>
#include <cstdint>
#include <string>
#include <vector>
#include <array>
#include <iostream>
#include <fstream>

using namespace std;
//}}}

// const
constexpr bool kSwitchDebug = false;
constexpr bool kCmdLineDebug = false;
constexpr bool kObjFileDebug = true;
constexpr bool kPass1Debug = true;
constexpr bool kPass2Debug = true;

enum eSwitches { eChat, eDebug, eBin };
enum eRecordType { eNone, eModule, eESD, EText, eEOM };

// vars
  //{{{
  class cObjRecord {
  public:
    cObjRecord() {}
    virtual ~cObjRecord() = default;

    //{{{
    bool getRO (ifstream& stream) {

      mLength = stream.get();

      uint8_t b = stream.get();
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

  private:
    uint8_t mLength = 0;
    eRecordType mType = eNone;
    array <uint8_t,255> mBlock = {0};
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
  void processSwitch (const string& line, array <int,3>& switches) {

    if (kSwitchDebug)
      printf ("processSwitch %s\n", line.c_str());

    if (line == "chat")
      switches[eChat] = 1;
    else if (line == "debug")
      switches[eDebug] = 1;
    else if (line == "bin")
      switches[eBin] = 1;
    }
  //}}}
  //{{{
  void processSwitches (const string& line, array <int,3>& switches) {

    if (kSwitchDebug)
      printf ("processSwitches %s\n", line.c_str());

    // parse into individual switches, stripping out /
    size_t start = 1;
    size_t found = line.find ('/', start);
    while (found != string::npos)  {
      processSwitch (line.substr (start, found-start), switches);
      start = found + 1;
      found = line.find ('/', start);
      }

    processSwitch (line.substr (start, found), switches);
    }
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

    if (kCmdLineDebug)
      printf ("processInclude %s\n", line.c_str());
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
  void pass1File (const string& fileName, array <int,3>& switches) {

    if (kPass1Debug)
      printf ("pass1file %s\n", fileName.c_str());

    ifstream objFileStream (fileName, ifstream::in);

    bool eom = false;
    cObjRecord objRecord;
    while (!eom) {
      eom = objRecord.getRO (objFileStream);
      objRecord.report();
      }

    objFileStream.close();
    }
  //}}}

  //{{{
  void pass1 (const vector <string>& objFiles, array <int,3>& switches) {

    for (auto& objFile : objFiles)
      pass1File (objFile.c_str(), switches);
    }
  //}}}
  //{{{
  void pass2 (const vector <string>& objFiles, array <int,3>& switches) {

    for (auto& objFile : objFiles)
      printf ("pass2 %s\n", objFile.c_str());
    }
  //}}}

//{{{
int main (int numArgs, char* args[]) {

  // get cmd line
  string cmdFileName;
  array <int,3> switches = {0};

  for (int i = 1; i < numArgs; i++)
    if (args[i][0] == '/')
      processSwitches (args[i], switches);
    else
      cmdFileName = args[i];
  if (kCmdLineDebug)
    printf ("cmdFileName %s\n", cmdFileName.c_str());

  if (cmdFileName.empty()) {
    //{{{  no .cmd filename - error, exit
    printf ("no .cmd file specified\n");
    return 1;
    }
    //}}}

  // get objFiles from .cmd file
  vector <string> objFiles;
  string line;

  ifstream cmdFileStream (cmdFileName + ".cmd", ifstream::in);
  while (getline (cmdFileStream, line)) {
    if (line[0] == '/')
      processSwitches (line, switches);
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

  // process object files
  pass1 (objFiles, switches);
  pass2 (objFiles, switches);

  return 0;
  }
//}}}
