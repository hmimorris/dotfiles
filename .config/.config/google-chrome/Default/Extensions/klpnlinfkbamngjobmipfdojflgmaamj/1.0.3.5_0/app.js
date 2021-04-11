var urlForDownload;

var AppID = "phpfkeikhkofikgikkgkhlafelljmpii";
// Debug
//var AppID = "ifeambincbdnajnbhkgkjjomnamonaof";

//-----------------------------------------------------------------------------

function extractFileName(downloadLink)
{
	return downloadLink.substr(downloadLink.lastIndexOf("/")+1);
}

//-----------------------------------------------------------------------------

function createUniqueFile(fileName, counter, Entry, callback)
{
	var fn = fileName;
	if (counter !=0)
	{
		fn = "(" + counter + ")" + fileName;
	}

	Entry.getFile(
		fn, 
		{create : true, exclusive: true},
		function(FileEntry)
		{
			callback(FileEntry);
		},
		function(e)
		{
			createUniqueFile(fileName,counter+1,Entry,callback);
		}
	);
}

//-----------------------------------------------------------------------------

function onPickDir(Entry)
{

	for (var i=0; i<urlForDownload.length; i++)
	{
		(function(cntr)
		{
			var Url = urlForDownload[cntr].url;
			var fileName = extractFileName(urlForDownload[cntr].url);
			//Entry.getFile(fileName, {create : true},
			createUniqueFile(fileName,0,Entry,
			function(FileEntry)
			{
				var req = new XMLHttpRequest();
				req.onreadystatechange = function () 
				{
					if ((req.readyState == 4) && (req.status == 200)) {
						FileEntry.createWriter(function(writer) 
						{
						    writer.onwriteend = function()
						    {
						    	chrome.runtime.sendMessage(
								AppID,
								fileName);
						    };

						    writer.write(req.response);

						});
					}
				}
				req.open("GET", Url, true);//some_binary_file.ext
				req.responseType = "blob";
				req.send(null);			
			});
		})(i);
	}
	chrome.app.window.current().hide();
}

//-----------------------------------------------------------------------------

function onRecieveMessage(Message,Sender,Response)
{
	if (Message.command == "url")
	{
		urlForDownload = Message.data;

		  var accepts = [{
		    mimeTypes: ['text/*'],
		    extensions: ['js', 'css', 'txt', 'html', 'xml', 'tsv', 'csv', 'rtf']
		  }];
		chrome.fileSystem.chooseEntry(
	 	{type: 'openDirectory', accepts: accepts}, 
	 	onPickDir);
	}
	Response({"result":"OK"});
}

//-----------------------------------------------------------------------------

chrome.runtime.onMessageExternal.addListener(onRecieveMessage);

chrome.runtime.onConnectExternal.addListener(function(port) {
  port.onMessage.addListener(function(msg) {
    // See other examples for sample onMessage handlers.
  });
});