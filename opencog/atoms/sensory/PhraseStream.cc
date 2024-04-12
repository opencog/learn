/*
 * opencog/atoms/sensory/PhraseStream.cc
 *
 * Copyright (C) 2020 Linas Vepstas
 * All Rights Reserved
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <errno.h>
#include <string.h> // for strerror()

#include <opencog/util/exceptions.h>
#include <opencog/util/oc_assert.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/value/ValueFactory.h>
#include "PhraseStream.h"

using namespace opencog;

PhraseStream::PhraseStream(Type t, const std::string& str)
	: LinkStreamValue(t)
{
	OC_ASSERT(nameserver().isA(_type, PHRASE_STREAM),
		"Bad PhraseStream constructor!");
	init(str);
}

PhraseStream::PhraseStream(const std::string& str)
	: LinkStreamValue(PHRASE_STREAM)
{
	init(str);
}

PhraseStream::~PhraseStream()
{
printf ("stream dtor\n");
}

// Attempt to open the URL for reading.
void PhraseStream::init(const std::string& url)
{
	_fh = nullptr;
	if (0 != url.compare(0, 7, "file://"))
		throw RuntimeException(TRACE_INFO,
			"Unsupported URL \"%s\"\n", url.c_str());

	// Ignore the first 6 chars "file:/"
	const char* fpath = url.substr(6).c_str();
	_fh = fopen(fpath, "r");

	if (nullptr == _fh)
	{
		int norr = errno;
		char buff[80];
		buff[0] = 0;
		// Apparently, we are getting the Gnu version of strerror_r
		// and not the XSI version. I suppose it doesn't matter.
		char * ers = strerror_r(norr, buff, 80);
		throw RuntimeException(TRACE_INFO,
			"Unable to open URL \"%s\"\nError was \"%s\"\n",
			url.c_str(), ers);
	}
}

// ==============================================================

// This will ...
void PhraseStream::update() const
{
printf("duude dte\n");
	_value.emplace_back(createNode(CONCEPT_NODE, "foocon"));
}

// ==============================================================

bool PhraseStream::operator==(const Value& other) const
{
	// Derived classes use this, so use get_type()
	if (get_type() != other.get_type()) return false;

	if (this == &other) return true;

	return LinkValue::operator==(other);
}

// ==============================================================

// Adds factory when library is loaded.
DEFINE_VALUE_FACTORY(PHRASE_STREAM, createPhraseStream, std::string)
