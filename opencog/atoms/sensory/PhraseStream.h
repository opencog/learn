/*
 * opencog/atoms/sensory/PhraseStream.h
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

#ifndef _OPENCOG_PHRASE_STREAM_H
#define _OPENCOG_PHRASE_STREAM_H

#include <stdio.h>
#include <opencog/atoms/value/LinkStreamValue.h>
#include <opencog/atoms/sensory-types/sensory_types.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * PhraseStreams provide a stream of PhraseNodes from text file and,
 * more generally, from unix socket sources. This is experimental;
 * It might be better to have e.g. a TextStream inheriting from
 * QueueValue (for threading) and return StringValue streams instead
 * of PhraseNodes. I dunno, remains unclear.
 */
class PhraseStream
	: public LinkStreamValue
{
protected:
	PhraseStream(Type t, const std::string&);
	void init(const std::string&);
	virtual void update() const;

	FILE* _fh;

public:
	PhraseStream(const std::string&);
	virtual ~PhraseStream();
	virtual bool operator==(const Value&) const;
};

typedef std::shared_ptr<PhraseStream> PhraseStreamPtr;
static inline PhraseStreamPtr PhraseStreamCast(ValuePtr& a)
	{ return std::dynamic_pointer_cast<PhraseStream>(a); }

template<typename ... Type>
static inline std::shared_ptr<PhraseStream> createPhraseStream(Type&&... args) {
   return std::make_shared<PhraseStream>(std::forward<Type>(args)...);
}

/** @}*/
} // namespace opencog

#endif // _OPENCOG_PHRASE_STREAM_H
