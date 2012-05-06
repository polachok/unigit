#include <iostream>
#include <iomanip>
#include <fstream>
#include <algorithm>
#include <string>
#include <sstream>
#include <vector>

using namespace std;

class Dupstream : public ostream {
	class DupstreamBuf : public filebuf {
	public:
		DupstreamBuf(const char *filename) {
			filebuf::open(filename, ios::out);
	       	}
		~DupstreamBuf() {
			filebuf::close();
		}
		virtual int sync() {
			for(char *c = pbase(); *c != '\n'; c++) {
				cout << *c;
			}
			cout << endl;
			return filebuf::sync();
		}
	};
	public:
	streamsize width(streamsize n) { cout.width(n); ostream::width(n); }
	fmtflags setf(fmtflags fl) { cout.setf(fl); ostream::setf(fl); }
	fmtflags unsetf(fmtflags fl) { cout.unsetf(fl); ostream::unsetf(fl); }
	Dupstream(const char *fname) : ostream(new DupstreamBuf(fname)), ios(0) {};
	~Dupstream(void) { delete rdbuf(); }
};

Dupstream cdup("log.txt");

class Record;
const string& Artist(Record &);
const string& Album(Record &);
const string& Title(Record &);
const int& Year(Record &);
const int& Plays(Record &);

#define LENGTH(s) (sizeof(s)/sizeof(s[0]))

/* value types */
enum { Str, Int };
enum { Lt = -1, Eq = 0, Gt = 1 };

struct fn {
	const char *name;
	int width;
	int flags;
	int type;
	void *get;
} field[] = {
	{ "Artist", 20, ios::left,  Str, (void*)Artist },
	{ "Album",  20, 0, 	    Str, (void*)Album },
	{ "Title",  20, 0, 	    Str, (void*)Title },
	{ "Year",   5,  0, 	    Int, (void*)Year },
	{ "Plays",  6,  ios::right, Int, (void*)Plays },
};

#define FIELD(name_, type_)  \
	class name_ { \
		type_ value; \
		public: \
		template <typename String> name_(String &s) { value = s; }; \
		name_() {}; \
		name_(const name_ &copy) { value = copy.value; } \
		template <typename String> void operator= (String &s) { value = s; }; \
		const type_& operator() (void) { return value; } \
		bool operator > (name_ &that) { return (this->value > that.value); }\
	}

class Record {
	FIELD (Artist, string) r_artist;
	FIELD (Album, string) r_album;
	FIELD (Title, string) r_title;
	FIELD (Year, int) r_year;
	FIELD (Plays, int) r_plays;
	public:
	template <typename String> Record(String a, String b, String t, int y, int p) {
		r_artist = a;
		r_album = b;
		r_title = t;
		r_year = y;
		r_plays = p;
	}
	Record() {};
	friend void printRecord(ostream&, Record&);
	friend const string& Artist(Record&);
	friend const string& Album(Record&);
	friend const string& Title(Record&);
	friend const int& Year(Record&);
	friend const int& Plays(Record&);
	friend ifstream& operator >> (ifstream&, Record&);
	friend ofstream& operator << (ofstream&, Record&);
};

const string& Artist(Record &r) {
	return r.r_artist();
}

const string& Album(Record &r) {
	return r.r_album();
}

const string& Title(Record &r) {
	return r.r_title();
}

const int& Year(Record &r) {
	return r.r_year();
}

const int& Plays(Record &r) {
	return r.r_plays();
}

template <typename Value> int compare(Value (*field)(Record&), Record &a, Record& b) {
	if (field(a) < field(b))
		return -1;
	if (field(a) == field(b))
		return 0;
	return 1;
}

template <typename Value> int compare(Value (*field)(Record&), Record &r, string s) {
	stringstream ss;
	ss << field(r);
	if (ss.str() < s)
		return -1;
	if (ss.str() == s)
		return 0;
	return 1;
}

class Compare {
	const char *s;
	int v;
	public: 
	Compare(const char *s, int v) { this->s = s; this->v = v; };
	template <typename RecordOrString> bool operator() (Record a, RecordOrString b) {
		string& (*gets)(Record&); 
		int& (*geti)(Record&); 
		for(int i=0; i < LENGTH(field); i++) {
			if (strcmp(this->s, field[i].name) == 0) {
				gets = (string& (*)(Record&))field[i].get;
				geti = (int& (*)(Record&))field[i].get;
				if (field[i].type == Int)
					return (v == compare(geti, a, b));
				else
					return (v == compare(gets, a, b));
			}
		}
		cerr << "Compare: wrong field" << s << endl;
		return 0;
	}
};

class Db {
	vector <Record> Records;
	const char *filename;
	ifstream inf;
	ofstream ouf;
	void read(void);
	void write(void);
	public:
	template <typename String> Db(String);
	~Db(void);
	void print(void);
	void header(void);
	void add(Record);
	void sort(void);
	void flush(void);
	template <typename String1, typename String2> void remove(String1, String2);
	template <typename String1, typename String2> void select(String1, String2);
	template <typename String> void sort(String);
};

template <typename String> Db::Db(String filename) {
	this->filename = filename;
	this->inf.open(filename);
	if (!inf)
		cerr << "Cannot open file " << filename << endl;
	this->read();
	this->inf.close();
}

Db::~Db(void) {
	flush();
}

void Db::flush(void) {
	this->ouf.open(filename);
	if (!ouf)
		cerr << "Cannot open file " << filename << endl;
	this->write();
	this->ouf.close();
}

void Db::read(void) {
	Record r;

	while (this->inf) {
		this->inf >> r;
		if (!this->inf.good())
			break;
		Records.push_back(r);
	}
}

void Db::write(void) {
	vector <Record>::iterator it;

	for(it = Records.begin(); it != Records.end(); it++)
		this->ouf << *it;
}

void Db::header(void) {
	for(int i = 0; i < LENGTH(field); i++) {
		if (field[i].flags)
			cdup.setf(ios_base::fmtflags(field[i].flags));
		cdup << setw(field[i].width) << field[i].name;
	}
	cdup << endl;
	for(int i = 0; i < 80; i++)
		cdup << '-';
	cdup << endl;
	cdup.unsetf(ios::left | ios::right);
}


void Db::print(void) {
	vector <Record>::iterator it;

	header();
	for(it = Records.begin(); it != Records.end(); it++)
		printRecord(cdup, *it);
}

void Db::add(Record r) {
	Records.push_back(r);
}

template <typename String1, typename String2> void Db::remove(String1 field, String2 value) {
	Compare p(field, Eq);
	vector <Record>::iterator it;
	bool found = 0;

	it = Records.begin();
	while (it != Records.end()) {
		if ((it = search (it, Records.end(), &value, &value+1, p)) 
				!= Records.end()) {
			found = 1;
			Records.erase (it);
		}
	}
	if (!found)
		cerr << "Nothing found by your request " << value << endl;
}

template <typename String1, typename String2> void Db::select(String1 field, String2 value) {
	Compare p(field, Eq);
	vector <Record>::iterator it;
	bool found = 0;

	header();
	it = Records.begin();
	while (it != Records.end()) {
		if ((it = search (it, Records.end(), &value, &value+1, p)) 
				!= Records.end()) {
			printRecord(cdup, *it);
			found = 1;
			it++;
		}
	}
	if (!found)
		cerr << "Nothing found by your request " << value << endl;
}

template <typename String> void Db::sort(String field) {
	Compare cmp(field, Lt);

	std::sort(Records.begin(), Records.end(), cmp);
	this->print();
}

ofstream& operator << (ofstream &s, Record &r) {
	s << Artist(r) << endl << Album(r) << endl
	  << Title(r) << endl << Year(r) << endl << Plays(r) << endl;
	return s;
}

void printRecord(ostream &s, Record &r) {
	string& (*gets)(Record&); 
	int& (*geti)(Record&); 

	for(int i = 0; i < LENGTH(field); i++) {
		if (field[i].flags)
			cdup.setf(ios_base::fmtflags(field[i].flags));
		gets = (string& (*)(Record&))field[i].get;
		geti = (int& (*)(Record&))field[i].get;
		s << setw(field[i].width);
		if (field[i].type == Int)
			s << geti(r);
		else
			s << gets(r);
	}
	s << endl;
	s.unsetf(ios::left | ios::right);
}

ifstream& operator >> (ifstream &is, Record &r) {
	string s;
	stringstream ss;
	int i;

	getline(is, s);
	r.r_artist = s;
	getline(is, s);
	r.r_album = s;
	getline(is, s);
	r.r_title = s;
	getline(is, s);
	ss << s;
	ss >> i;
	r.r_year = i;
	/* clear the stream: need both */
	ss.str("");
	ss.clear();

	getline(is, s);
	ss << s;
	ss >> i;
	r.r_plays = i;
	return is;
}

class Menu {
	template <typename String> void ask(const char*, String&);
	void ask(const char*, int&);
	template <typename String> void askfield(const char*, String&);
	public:
	void draw(void);
	void print(Db&);
	void add(Db&);
	void select(Db&);
	void sort(Db&);
	void remove(Db&);
};

template <typename String> void Menu::askfield(const char *prompt, String &value) {
	int c = -1;

	cdup << endl;
	cdup << "Fields: " << endl;
	for(int i = 0; i < LENGTH(field); i++)
		cdup << " " << i << "." << field[i].name << endl;
	ask(prompt, c);
	if ((c >= 0) && (c < LENGTH(field)))
		value = field[c].name;
	else
		value = "";
	cdup << value << endl;
}

template <typename String> void Menu::ask(const char *prompt, String &value) {
	cdup << prompt << ": " << endl;
	getline(cin, value);
}

void Menu::ask(const char *prompt, int &value) {
	string s;
	stringstream ss;

	cdup << prompt << ": " << endl;
	getline(cin, s);
	ss << s;
	ss >> value;
}

void Menu::print(Db &db) {
	db.print();
}

void Menu::add(Db &db) {
	string artist, album, title;
	int year, plays;

	ask("Artist", artist);
	ask("Album", album);
	ask("Title", title);
	ask("Year", year);
	ask("Plays", plays);
	db.add(Record(artist, album, title, year, plays));
}

void Menu::select(Db &db) {
	string v;
	const char *f;

	askfield("Select by", f);
	if (f=="") {
		cerr << "Wrong field, try again" << endl;
		select(db);
	}
	ask("Value", v);
	db.select(f, v);
}

void Menu::sort(Db &db) {
	const char *f;
	int c;

	askfield("Sort by", f);
	if (f=="") {
		cerr << "Wrong field, try again" << endl;
		sort(db);
	}
	db.sort(f);
}

void Menu::remove(Db &db) {
	string v;
	const char *f;

	askfield("Delete by", f);
	if (f=="") {
		cerr << "Wrong field, try again" << endl;
		remove(db);
	}
	ask("Value", v);
	db.remove(f, v);
}

void Menu::draw(void) {
	cdup << endl;
	cdup << "1.Print file contents" << endl;
	cdup << "2.Add new data" << endl;
	cdup << "3.Select" << endl;
	cdup << "4.Sort" << endl;
	cdup << "5.Delete" << endl;
	cdup << "q.Quit" << endl;
}

int main() {
	char choice;

	Db db("test.db");
	Menu menu;
	while (1) {
		menu.draw();
		cin.get(choice);
		cin.seekg(0, ios::end);
		switch (choice) {
			case '1':
				menu.print(db);
				break;
			case '2':
				menu.add(db);
				break;
			case '3':
				menu.select(db);
				break;
			case '4':
				menu.sort(db);
				break;
			case '5':
				menu.remove(db);
				break;
			case 'q':
				db.flush();
				return(0);
		}
	}
}
