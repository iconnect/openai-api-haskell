# API types for OpenAI and Azure REST services

This project was born as a fork of [openai-hs](https://github.com/agrafix/openai-hs) but over the months
it accumulated a number of distinct features which weren't easily reconcileable with the main repo, notably:

* Support for version 2 (i.e. V2) of the API (Assistants, Threads, Vector Stores, etc);
* Support for streaming via the servant event stream library;
