module TestText where

loremIpsum :: String
loremIpsum = 
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce facilisis laoreet lectus ut dignissim. Praesent semper enim non turpis sollicitudin, sit amet fermentum ligula lobortis. Integer vel enim tristique, congue lectus ultricies, eleifend tellus. Nunc dapibus faucibus consectetur. Ut molestie facilisis purus, et tempus nulla elementum quis. Phasellus ut est tincidunt, molestie leo eget, rutrum felis. Vestibulum eu consectetur nunc, ut ullamcorper quam. Vestibulum mollis rutrum tellus, ut sodales mauris tincidunt sed. Praesent ultrices vulputate orci, ultrices volutpat tellus luctus eget. Praesent sed magna ut lacus gravida pellentesque non in elit. Integer arcu eros, elementum quis nunc vel, ultrices blandit velit. Phasellus consequat ultrices tellus eget dignissim. Morbi et commodo eros, id tempor purus. Mauris accumsan luctus dolor. Curabitur interdum lacus magna, ut consequat mi cursus et. Quisque sit amet diam hendrerit, viverra ex non, feugiat nisi. "

loremIpsumMarkFree :: String
loremIpsumMarkFree =
  "Lorem ipsum dolor sit amet consectetur adipiscing elit Fusce facilisis laoreet lectus ut dignissim Praesent semper enim non turpis sollicitudin sit amet fermentum ligula lobortis Integer vel enim tristique congue lectus ultricies eleifend tellus Nunc dapibus faucibus consectetur Ut molestie facilisis purus et tempus nulla elementum quis Phasellus ut est tincidunt molestie leo eget rutrum felis Vestibulum eu consectetur nunc ut ullamcorper quam Vestibulum mollis rutrum tellus ut sodales mauris tincidunt sed Praesent ultrices vulputate orci ultrices volutpat tellus luctus eget Praesent sed magna ut lacus gravida pellentesque non in elit Integer arcu eros elementum quis nunc vel ultrices blandit velit Phasellus consequat ultrices tellus eget dignissim Morbi et commodo eros id tempor purus Mauris accumsan luctus dolor Curabitur interdum lacus magna ut consequat mi cursus et Quisque sit amet diam hendrerit viverra ex non feugiat nisi"

loremIpsum2 :: String 
loremIpsum2 =
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed eget dui non diam finibus efficitur. Curabitur imperdiet erat dapibus dui semper, nec commodo ante hendrerit. Suspendisse fringilla velit nec mi pulvinar tempus. Suspendisse fringilla maximus sem aliquet convallis. Nam in fringilla justo. Etiam et tristique urna."
  
loremIpsum3 :: String 
loremIpsum3 = 
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vestibulum rutrum et lacus a volutpat. Ut id mauris ipsum. Nullam pulvinar elit nulla, sed pulvinar augue interdum quis. Phasellus dolor purus, tempor sit amet justo a, laoreet viverra libero. Proin accumsan scelerisque arcu et ornare. Etiam quis sollicitudin lorem. Nunc commodo. "
  
loremIpsum2Debug :: String 
loremIpsum2Debug =
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed eget dui non diam finibus efficitur. Curabitur"
  
smallWords :: String
smallWords = concat $ replicate 500 "abcde "