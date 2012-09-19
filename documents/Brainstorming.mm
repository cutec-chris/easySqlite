<map version="0.9.0">
<!-- To view this file, download free mind mapping software FreeMind from http://freemind.sourceforge.net -->
<node CREATED="1348055176352" ID="ID_1417673350" MODIFIED="1348055231082">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p style="text-align: center">
      SqliteI
    </p>
  </body>
</html></richcontent>
<font BOLD="true" NAME="SansSerif" SIZE="12"/>
<node CREATED="1348055236092" ID="ID_1857945472" MODIFIED="1348055239832" POSITION="right" TEXT="Features">
<font BOLD="true" NAME="SansSerif" SIZE="12"/>
<node CREATED="1348055240992" ID="ID_677494973" MODIFIED="1348056745080" TEXT="Zugriff auf ResultFields per Name">
<icon BUILTIN="button_ok"/>
</node>
<node CREATED="1348055670986" ID="ID_51106416" MODIFIED="1348056157827" TEXT="Automapping f&#xfc;r Selects, Inserts und Updates">
<richcontent TYPE="NOTE"><html>
  <head>
    
  </head>
  <body>
    <p>
      <b>Insert</b>
    </p>
    <p>
      Aufruf: &quot;InsertObject(AObject: TObject; Table: String);&quot;
    </p>
    <p>
      Beim Insert werden alle Published-Properties eines Objektes ausgewertet und ihr Inhalt in Tabellenfelder gleichen Namens geschrieben.
    </p>
    <p>
      
    </p>
    <p>
      <b>Update </b>
    </p>
    <p>
      Aufruf: &quot;UpdateObject(AObject: TObject; Table, ConditionalField: String);
    </p>
    <p>
      Beim Update werden alle Published-Properties eines Objektes ausgewertet und ihr Inhalt in Tabellenfelder gleichen Namens geschrieben. Die Where-Bedingung wird mit der Property &quot;ConditionalField&quot; verkn&#252;pft.
    </p>
    <p>
      
    </p>
    <p>
      <b>Select</b>
    </p>
    <p>
      Aufruf: &quot;SelectObject(ClassType: TClass; Table, WhereCondition: String);&quot;
    </p>
  </body>
</html></richcontent>
</node>
</node>
</node>
</map>
