/*
 * opencog/atoms/sensory/FileReadNode.h
 *
 * Copyright (C) 2015 Linas Vepstas
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

#ifndef _OPENCOG_FILE_READ_NODE_H
#define _OPENCOG_FILE_READ_NODE_H

#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/sensory-types/sensory_types.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 *
 * FileReadNode class. Opens files/sockets for reading.
 */

class FileReadNode : public Node
{
protected:
	void init(void);

public:
	// Please to NOT use this constructor!
	FileReadNode(Type, const std::string&&);

public:
	FileReadNode(const std::string&&);

	FileReadNode(FileReadNode&) = delete;
	FileReadNode& operator=(const FileReadNode&) = delete;

	virtual bool is_executable() const { return true; }
	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

NODE_PTR_DECL(FileReadNode)
#define createFileReadNode CREATE_DECL(FileReadNode)

/** @}*/
}

extern "C" {
void opencog_sensory_init(void);
};

#endif // _OPENCOG_FILE_READ_NODE_H
