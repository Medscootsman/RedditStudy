#include<iostream>
#include<fstream>
#include<string>
#include"rapidjson/document.h"
#include"rapidjson/writer.h"
#include"rapidjson/stringbuffer.h"
#include<chrono>

using namespace std;
using namespace rapidjson;

int main(int argc, char *argv[]){

	if(argc < 2){
		cout << "You need to specify the subreddit to parse" << endl;
		cout << "Syntax: ./subreddit_parser <subreddit name>" << endl;
		return 0;
	}

	string tempString = "";

	ofstream outputFile(argv[1] + string(".json"));

	auto startTime = chrono::system_clock::now();
	time_t start_time = std::chrono::system_clock::to_time_t(startTime);
	
	cout << endl << "Starting at " << ctime(&start_time) << endl;
	cout << "Parsing subreddit: " << argv[1] << endl;
	cout << "Will be saved to " << argv[1] << ".json in the current directory." << endl << endl;
	cout << "Completion:" << endl;
	cout << "0%---------------------------50%--------------------------100%" << endl;
	
	for(int i = 0; i < 122; i++){
		if(i % 2 == 0) cout << "|" << flush;
		ifstream inputFile("sample" + to_string(i) + ".json");

		while(getline(inputFile, tempString)){

			Document d;
			d.Parse(tempString.c_str());

			if(d["subreddit"] == argv[1]){
				outputFile << tempString << endl;
			}

			//if(tempString.find("\"subreddit\":\"australia\"") < tempString.length()){
				//outputFile << tempString << endl;
			//}

		}
		inputFile.close();
	}
	outputFile.close();

	auto endTime = chrono::system_clock::now();
	time_t end_time = std::chrono::system_clock::to_time_t(endTime);
	
	cout << endl << endl << "Finished parsing at " << ctime(&end_time) << endl;

	return 0;
}
