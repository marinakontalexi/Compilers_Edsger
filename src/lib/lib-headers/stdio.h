/*    stdio.h for Edsger     */
/* I/O Function Declarations */

/*     Output Functions      */
void writeInteger (int n);
void writeBoolean (bool b);
void writeChar (char c);
void writeReal (double d);
void writeString (char * s);
void print (int n);

/*      Input Functions     */
int readInteger ();
bool readBoolean ();
char readChar ();
double readReal ();
void readString (int size, char * s);
int read();