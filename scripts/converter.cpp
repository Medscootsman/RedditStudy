#include <iostream>
#include <string>
#include <fstream>
#include <vector>
#include <sstream>

using namespace std;

int main(){

	//Read in all comments into 2d vector
	ifstream input;
	input.open("data.txt");
	string line;
	vector<vector<string> > matrix;

	while(getline(input,line)){
		istringstream s(line);
		string field;
		vector<string> temp;
		while (getline(s, field,',')){
			temp.push_back(field);
		}
		matrix.push_back(temp);
	}

	input.close();

	//Read in dictionary
	ifstream input2;
	input2.open("dataword.txt");
	vector<string> dictionary;
	
	while(getline(input2,line)){
		dictionary.push_back(line);
	}
	input2.close();

	//Create empty 2d vector
	vector<vector<int> > matrix2;
	vector<int> temp;
	for(int i = 0; i < 128; i++){
		temp.push_back(0);
	}

	for(int i = 0; i < 40000; i++){
		matrix2.push_back(temp);
	}

	for(int i = 0; i < dictionary.size(); i++){
		dictionary[i] = dictionary[i].substr(0,dictionary[i].length()-1);
	}
	int trigger = 0;
	for(int i = 0; i < matrix.size(); i++){
		for(int j = 0; j < matrix[0].size(); j++){
			if(matrix[i][j] == "") break;
			trigger = 0;
			for(int k = 0; k < 10000; k++){
				if(matrix[i][j].compare(dictionary[k])==0){
					trigger = 1;
					//cout << k << " - " << dictionary[k] << endl;
					matrix2[i][j] = k;
				}
				if(trigger == 0) matrix2[i][j] = 10000;
			}
		}
	}
	
	ofstream output;
	output.open("post_and_comments.txt");
	
	for(int i = 0; i < matrix.size(); i++){
		for(int j = 0; j < 128; j++){
			output << matrix2[i][j];
			if(j < 127){output << ",";}
		}
		output << endl;
	}

	output.close();

	return 0;
}
