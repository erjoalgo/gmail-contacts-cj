Standalone program that maintains a local sqlite cache of (name . email) pairs by extracting
senders and recipients from email messages in an IMAP-accessible inbox.

Provides additional emacs utility to enable completing read of email addresses by name/email within gnus message mode.

# Features

-   Not too slow: extracts only TO/FROM/CC/BCC fields. Does not fetch or store message bodies, attachments or other heavier fields.
-   Batches of emails processed in parallel
-   Fetches newst messages first. It is not necessary to process entire inbox to have usable completion
-   Not gmail specific, works with any imap server

# Building/Installation

-   Clone this repository, then run `lein uberjar`. See Bugs
-   Alternatively, use a standalone jar from the releases

# Usage and options

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />

<col  class="left" />

<col  class="left" />

<col  class="left" />
</colgroup>
<tbody>
<tr>
<td class="left">Option</td>
<td class="left">Default</td>
<td class="left">Description</td>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">-e, &#x2013;email EMAIL</td>
<td class="left">me@gmail.com</td>
<td class="left">email address</td>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">-d, &#x2013;db DB</td>
<td class="left">*home/user*.imap-contacts.db</td>
<td class="left">path to sqlite db</td>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">-m, &#x2013;max-results MAX</td>
<td class="left">600</td>
<td class="left">max results to fetch, default 600, 0 for infinite</td>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">-p, &#x2013;passwd-file PASSWD\_FN</td>
<td class="left">path to file containing app specific pass. user is prompted if not provided</td>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">-n, &#x2013;newline</td>
<td class="left">&#xa0;</td>
<td class="left">display one message per line instead of continuous progress indicator</td>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">-q, &#x2013;quiet</td>
<td class="left">quiet</td>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">-s, &#x2013;imap-protocol-host-port IMAP\_SERVER</td>
<td class="left">["https" "imap.gmail.com" 993]</td>
<td class="left">url for for imap server including protocol, host, port, example 'https://imap.gmail.com:993'</td>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
</tr>
</tbody>
</table>

# Bugs

This project depends on features of [clojure-mail](https://github.com/owainlewis/clojure-mail) unreleased as of this writing. A workaround is to clone the latest version from master, run `lein install` locally, and build this project locally. Alternatively, use the standalone jar.