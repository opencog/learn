# REST Bridge for AtomSpace / LLM

This module provides a lightweight HTTP interface that allows external clients
(e.g., LLMs, Python applications, or other services) to submit **Atomese /
Scheme programs** to a running **OpenCog CogServer** and receive the raw
evaluation output. The bridge is intentionally minimal and functions as a
transport layer only; all reasoning and memory operations occur entirely
inside the AtomSpace.

The project establishes the infrastructure needed to integrate symbolic
reasoning systems with neural language models for research in
**neuro-symbolic AI**. Higher-level cognitive agents and orchestration logic
are planned but are not part of the current implementation.

---

## Architecture

LLM Prompt
   ↓
REST → Atomese generation
   ↓
Symbolic inference in AtomSpace
   ↓
Graph results back to LLM
   ↓
LLM reflection / critique
   ↓
Updated AtomSpace memory

---

## Repository Contents

- **server.py** – FastAPI REST server that maintains a telnet connection to
  CogServer and forwards Scheme commands.
- **atomese_client.scm** – Placeholder Scheme helper module for future
  procedural abstractions and reasoning workflows.
- **examples/clinical_demo.md** – Conceptual research example describing
  projected LLM + Atomese co-reasoning use cases.

---

## Current Implementation Status

### Implemented

- ✅ REST → CogServer telnet bridge  
- ✅ `/atomese` HTTP POST endpoint  
- ✅ Live execution of Atomese/Scheme inside AtomSpace  
- ✅ Swagger/OpenAPI interactive documentation  
- ✅ Verified end-to-end execution with atom creation and evaluation feedback

### Not Yet Implemented

- ❌ LLM orchestration or agent logic  
- ❌ Structured memory management for dialog or tasks  
- ❌ Reasoning pipelines or policy modules  
- ❌ Semantic parsing or response post-processing  
- ❌ Security and authentication layers for production use

This repository provides **infrastructure-only functionality**; it does not
constitute an autonomous AI or reasoning agent.

---

## Requirements

### Operating System

- Linux native installation or Windows **WSL2** environment.

### OpenCog

A full OpenCog toolchain must be built and installed:

- cogutil
- atomspace
- atomspace-storage
- atomspace-rocks
- cogserver

### Python

- Python ≥ 3.9  
- Dependencies:
  ```bash
  pip install fastapi uvicorn
