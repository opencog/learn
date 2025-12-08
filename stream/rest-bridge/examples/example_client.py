import telnetlib
import time

HOST = "localhost"
PORT = 17001  # CogServer telnet port

def main():
    print("Connecting to CogServer...")
    tn = telnetlib.Telnet(HOST, PORT, timeout=10)
    print("Connected.")

    # Wait for the initial 'opencog>' prompt
    print("Waiting for 'opencog>' prompt...")
    banner = tn.read_until(b"opencog> ", timeout=5)
    print("=== INITIAL BANNER ===")
    print(banner.decode(errors="ignore"))

    # Enter Scheme shell
    print("Sending 'scm'...")
    tn.write(b"scm\n")
    scm_prompt = tn.read_until(b"guile> ", timeout=5)
    print("=== ENTERED SCHEME SHELL ===")
    print(scm_prompt.decode(errors="ignore"))

    # Send a couple of Scheme commands
    print("Sending Scheme commands...")
    tn.write(b'(use-modules (opencog))\n')
    tn.write(b'(cog-set-atomspace! (cog-new-atomspace))\n')
    tn.write(b'(ConceptNode "FromPython")\n')
    tn.write(b"(cog-prt-atomspace)\n")

    # Exit Scheme shell
    tn.write(b".\n")

    time.sleep(1)
    output = tn.read_very_eager()
    print("=== SCHEME OUTPUT ===")
    print(output.decode(errors="ignore"))

    print("Sending 'quit'...")
    tn.write(b"quit\n")
    tn.close()
    print("Done.")

if __name__ == "__main__":
    main()
