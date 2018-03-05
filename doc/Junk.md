### This is a place to stash documentation that doesn't have a formal home yet.

# XML content

The "content" of each kind of xml value is what will be bound to a pattern variable (assuming a match)
that is "inside" that kind of xml value.

#### The rules:
* For any kind of tag with an "inside", the content of the tag is all the xml between the earliest `>` and the last `<`. If they use the special `/>` for an empty tag, the content will be the empty string.
  * Example: `<p>the inside</p>`
	  has content: `"the inside"`
  * Example: `<div><p class="a"></p></div>`
	  has content: `"<p class=\"a\"></p>"`
  * Example: `<div class="one">Some text</div>`
	  has content: `"Some text"`
  * Example: `<button/>`
	  has content: `""`

* Attributes' content is whatever the right side of the equals sign.
  * Example: `class="one"`
	has content: `"\"one\""`

* Entity's content is always itself. (an Entity is a symbol or numeric literal)
* PCData, and CData's content is always itself
* Comment's content is always itself.
* Prolog's content is all of the Miscs it contains.
