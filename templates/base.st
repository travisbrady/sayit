<html>
    <head>
        <title>I Got To Say This</title>
        <style>
            * {
                margin: 0; padding: 0
            }
            body {
                font-family: helvetica, arial, sans-serif;
                font-style: bold;
            }
            h1 {
                font-size: 5em;
            }
            #content {
                width: 700px;
                margin: auto
            }
            #sayeth {
                font-size: 8em;
                text-transform: uppercase;
                text-align: center
            }
        </style>
    </head>
    <body>
        <div id="content">
        $if(said)$
            <h1 id="sayeth">$said$</h1>
        $else$
            <h1>SAY IT<h1>
            <form method="post" name="sayform">
                <textarea rows="20"
                    cols="50" name="to_be_said"></textarea>
                <input type="submit"></input>
            </form>
        $endif$
        </div>
    </body>
</html>
