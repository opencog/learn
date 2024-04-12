/*
 * opencog/atoms/sensory/FileReadNode.cc
 *
 * Copyright (C) 2024 Linas Vepstas
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

#include <opencog/util/oc_assert.h>
#include <opencog/atoms/value/LinkStreamValue.h>
#include "FileReadNode.h"
#include "PhraseStream.h"

using namespace opencog;

// ============================================================
// Constructors

void FileReadNode::init(void)
{
	printf("yooooooo\n");
}

FileReadNode::FileReadNode(Type t, const std::string&& s)
   : Node(t, std::move(s))
{
   OC_ASSERT(nameserver().isA(_type, FILE_READ_NODE),
      "Bad FileReadNode constructor!");
	init();
}

FileReadNode::FileReadNode(const std::string&& s)
   : Node(FILE_READ_NODE, std::move(s))
{
	init();
}

// ============================================================

ValuePtr FileReadNode::execute(AtomSpace* as, bool silent)
{
	// Pass the URL to the stream; the stream will open it.
	return createPhraseStream(_name);
}

DEFINE_NODE_FACTORY(FileReadNode, FILE_READ_NODE)

void opencog_sensory_init(void)
{
	// Force shared lib ctors to run
};
