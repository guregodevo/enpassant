package com.chess

import org.scalatra._
import java.net.URL
import org.scalatra.scalate.ScalateSupport
import org.scalatra.fileupload.FileUploadSupport
import com.file.Control


class MainFilter extends ScalatraFilter with ScalateSupport with FileUploadSupport {
  
  get("/") {
    <html>
      <body>
        <h1>Hello, world!</h1>
        Say <a href="hello-scalate">hello to JustChess</a>.
                <br/>
                <form method="post" enctype="multipart/form-data">
                <input type="file" name="foo" />
                <input type="submit" />
                </form>
      </body>
    </html>
  }
  
  get("/hello/:name") {
    <html>
      <body>
        <h1>Hi, {params("name")}</h1>
        Say <a href="hello-scalate">hello to ChessMaster</a>.
      </body>
    </html>
  } 


  post("/") {
           val lines:List[String] = Control.using(fileParams("foo").getInputStream) { Control.readLines(_) }
       <html>
        <body>
         <h1>lines : </h1>
                <ul>{lines.map( i => <li>{i}</li> ) }</ul>
        </body>
       </html>
  }
          
  notFound {
    // If no route matches, then try to render a Scaml template
    val templateBase = requestPath match {
      case s if s.endsWith("/") => s + "index"
      case s => s
    }
    val templatePath = "/WEB-INF/scalate/templates/" + templateBase + ".scaml"
    servletContext.getResource(templatePath) match {
      case url: URL => 
        contentType = "text/html"
        templateEngine.layout(templatePath)
      case _ => 
        filterChain.doFilter(request, response)
    } 
  }
  
}
