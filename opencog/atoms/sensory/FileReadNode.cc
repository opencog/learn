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
#include <opencog/atoms/value/StringValue.h>
#include "FileReadNode.h"

using namespace opencog;

// ============================================================
// Constructors

FileReadNode::FileReadNode(Type t, const std::string&& s)
   : Node(t, std::move(s))
{
   OC_ASSERT(nameserver().isA(_type, FILE_READ_NODE),
      "Bad FileReadNode constructor!");
}

FileReadNode::FileReadNode(const std::string&& s)
   : Node(FILE_READ_NODE, std::move(s))
{
}

// ============================================================

ValuePtr FileReadNode::execute(AtomSpace* as, bool silent)
{
printf("duuuude ola\n");
	return createStringValue("foobar");
}

DEFINE_NODE_FACTORY(FileReadNode, FILE_READ_NODE)

void opencog_sensory_init(void)
{
	// Force shared lib ctors to run
};
