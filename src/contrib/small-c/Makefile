ifndef UNITY_ROOT
    $(warn UNITY_ROOT not set – most tests not available.)
endif

all:
	$(MAKE) -C src all
	$(MAKE) -C growroom all UNITY_ROOT="$(UNITY_ROOT)"

clean:
	$(MAKE) -C src clean
	$(MAKE) -C growroom clean UNITY_ROOT="$(UNITY_ROOT)"
