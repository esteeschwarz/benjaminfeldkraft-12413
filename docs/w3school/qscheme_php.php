<?php
//$myfile = fopen("qtemplate.xml", "a+") or die("FUCT! I forgot to tie my shoes!");
//fwrite($myfile, $txt);

//$xml=simplexml_load_string($myfile);
//foreach ($xml->children() as $child)
  //{
  //echo "Child node: " . $child . "<br>";
 // }

 $xmlDoc = new DOMDocument();
$xmlDoc->load("qtemplate.xml");

$x = $xmlDoc->documentElement;
foreach ($x->childNodes AS $item) {
  print $item->nodeName . " = " . $item->nodeValue . "<br>";
}
?>