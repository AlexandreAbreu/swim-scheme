// swim.cpp : Defines the entry point for the console application.
//


extern "C"
{
	int scheme_func ();
}


/**
 * 
 */
int shiftFromMask (int mask)
{
	int shift = 0;
	for (; mask != 0; mask >>= 1)
	{
		++shift;
	}
	return shift;
}

#include <sstream>

/**
 * 
 * 
 */
std::string
	prettyPrint (int value)
{
	const int fixnum_mask = 0x3;
	const int fixnum_shift = 0x2;
	const int fixnum_tag = 0x0;
	
	const int char_mask = 0xFF;
	const int char_shift = 8;
	const int char_tag = 15;
	
	const int boolean_mask = 0x7F;
	const int boolean_shift = 8;
	const int boolean_tag = 31;
	
	const int empty_list_mask = 0xFF;
	const int empty_list_shift = 8;
	const int empty_list_tag = 47;
	
	std::ostringstream result;
	if ((value & fixnum_mask) == fixnum_tag)
	{
		// fixnum
		result << (value >> fixnum_shift);
	}
	else if ((value & char_mask) == char_tag)
	{
		result << static_cast <std::ostringstream::char_type> (value >> char_shift);
	}
	else if ((value & boolean_mask) == boolean_tag)
	{
		result << (0 != (value >> boolean_shift));
	}
	else if ((value & empty_list_mask) == empty_list_tag)
	{
		result << "()";
	}
	return result.str();
}

#include <iostream>

int main(int argc, char* argv[])
{
	std::cout << prettyPrint (scheme_func ()) << std::endl;
	return 0;
}

